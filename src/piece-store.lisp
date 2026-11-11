;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT

(in-package #:cl-torrent)

(defconstant +peer-block-length+ 16384)

(defstruct piece-reference file pathname file-piece offset size expected-hash)
(defstruct (piece-store (:constructor %make-piece-store))
  torrent root pieces completed received-blocks)

(defun torrent-piece-count (torrent)
  (loop for file in (torrent-files torrent)
        sum (length (torrent-file-piece-hashes file))))

(defun torrent-piece-size (torrent piece)
  (unless (typep piece `(integer 0 (,(torrent-piece-count torrent))))
    (error "Piece index is out of range"))
  (let ((remaining piece) (piece-length (torrent-piece-length torrent)))
    (dolist (file (torrent-files torrent))
      (let ((count (length (torrent-file-piece-hashes file))))
        (if (< remaining count)
            (return (min piece-length
                         (- (torrent-file-length file) (* remaining piece-length))))
            (decf remaining count))))))

(defun safe-path-component-p (component)
  (and (stringp component) (plusp (length component))
       (not (member component '("." "..") :test #'string=))
       (not (find #\/ component)) (not (find #\\ component))))

(defun destination-path (root file)
  (unless (every #'safe-path-component-p (torrent-file-path file))
    (error "Unsafe path in torrent file tree"))
  (merge-pathnames (format nil "~{~A~^/~}" (torrent-file-path file))
                   (uiop:ensure-directory-pathname root)))

(defun ensure-sized-file (pathname length resume-p)
  (ensure-directories-exist pathname)
  (let ((current (and resume-p (probe-file pathname)
                      (with-open-file (stream pathname :direction :input
                                                       :element-type '(unsigned-byte 8))
                        (file-length stream)))))
    (unless (eql current length)
      (with-open-file (stream pathname :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create
                                       :element-type '(unsigned-byte 8))
        (when (plusp length)
          (file-position stream (1- length))
          (write-byte 0 stream))))))

(defun torrent-piece-references (torrent root)
  (let ((references nil) (piece-length (torrent-piece-length torrent)))
    (dolist (file (torrent-files torrent) (coerce (nreverse references) 'vector))
      (let ((pathname (destination-path root file)))
        (loop for local below (length (torrent-file-piece-hashes file))
              for offset = (* local piece-length)
              for size = (min piece-length (- (torrent-file-length file) offset))
              do (push (make-piece-reference
                        :file file :pathname pathname :file-piece local
                        :offset offset :size size
                        :expected-hash (aref (torrent-file-piece-hashes file) local))
                       references))))))

(defun hash-file-piece (reference piece-length)
  (with-open-file (stream (piece-reference-pathname reference)
                          :direction :input :element-type '(unsigned-byte 8))
    (file-position stream (piece-reference-offset reference))
    (let ((remaining (piece-reference-size reference)) (leaves nil))
      (loop while (plusp remaining)
            for count = (min remaining +bep52-block-length+)
            for bytes = (make-array count :element-type '(unsigned-byte 8))
            do (unless (= count (read-sequence bytes stream))
                 (error "Piece file ended unexpectedly"))
               (push (ironclad:digest-sequence :sha256 bytes) leaves)
               (decf remaining count))
      (let ((level (padded-hashes (coerce (nreverse leaves) 'vector)
                                  (/ piece-length +bep52-block-length+))))
        (loop while (> (length level) 1) do (setf level (parent-hashes level)))
        (aref level 0)))))

(defun piece-valid-p (store index)
  (let ((reference (aref (piece-store-pieces store) index)))
    (ironclad:constant-time-equal
     (hash-file-piece reference (torrent-piece-length (piece-store-torrent store)))
     (piece-reference-expected-hash reference))))

(defun open-piece-store (torrent root &key resume-p)
  "Create or resume filesystem storage for TORRENT under ROOT."
  (dolist (file (torrent-files torrent))
    (ensure-sized-file (destination-path root file) (torrent-file-length file) resume-p))
  (let* ((pieces (torrent-piece-references torrent root))
         (completed (make-array (length pieces) :element-type 'bit :initial-element 0))
         (received (map 'vector
                        (lambda (reference)
                          (make-array (ceiling (piece-reference-size reference)
                                               +peer-block-length+)
                                      :element-type 'bit :initial-element 0))
                        pieces))
         (store (%make-piece-store :torrent torrent :root root :pieces pieces
                                   :completed completed :received-blocks received)))
    (when resume-p
      (dotimes (index (length pieces))
        (when (piece-valid-p store index)
          (setf (aref completed index) 1)
          (fill (aref received index) 1))))
    store))

(defun open-blob-store (torrent pathname &key resume-p)
  "Create or resume a single-blob piece store that writes the whole blob to
PATHNAME, instead of deriving a destination under a root from the torrent name.
TORRENT must be a single-blob torrent.  The store is used with ACCEPT-BLOCK,
PIECE-STORE-COMPLETE-P and COMPLETED-PIECES exactly like OPEN-PIECE-STORE's."
  (unless (blob-torrent-p torrent)
    (error "open-blob-store requires a single-blob torrent"))
  (let* ((file (blob-torrent-file torrent))
         (piece-length (torrent-piece-length torrent))
         (pieces (let ((references nil))
                   (loop for local below (length (torrent-file-piece-hashes file))
                         for offset = (* local piece-length)
                         for size = (min piece-length (- (torrent-file-length file) offset))
                         do (push (make-piece-reference
                                   :file file :pathname pathname :file-piece local
                                   :offset offset :size size
                                   :expected-hash (aref (torrent-file-piece-hashes file) local))
                                  references))
                   (coerce (nreverse references) 'vector)))
         (completed (make-array (length pieces) :element-type 'bit :initial-element 0))
         (received (map 'vector
                        (lambda (reference)
                          (make-array (ceiling (piece-reference-size reference)
                                               +peer-block-length+)
                                      :element-type 'bit :initial-element 0))
                        pieces))
         (store (%make-piece-store :torrent torrent
                                   :root (uiop:pathname-directory-pathname pathname)
                                   :pieces pieces :completed completed
                                   :received-blocks received)))
    (ensure-sized-file pathname (torrent-file-length file) resume-p)
    (when resume-p
      (dotimes (index (length pieces))
        (when (piece-valid-p store index)
          (setf (aref completed index) 1)
          (fill (aref received index) 1))))
    store))

(defun completed-pieces (store)
  (copy-seq (piece-store-completed store)))

(defun piece-store-complete-p (store)
  (every (lambda (value) (= value 1)) (piece-store-completed store)))

(defun accept-block (store piece begin payload)
  "Write a requested block, publishing completion only after BEP 52 verification."
  (unless (typep piece `(integer 0 (,(length (piece-store-pieces store)))))
    (error "Piece index is out of range"))
  (let* ((reference (aref (piece-store-pieces store) piece))
         (size (piece-reference-size reference))
         (block (floor begin +peer-block-length+))
         (expected (min +peer-block-length+ (- size begin))))
    (unless (and (typep payload '(vector (unsigned-byte 8)))
                 (zerop (mod begin +peer-block-length+))
                 (<= 0 begin) (< begin size) (= expected (length payload)))
      (error "Block does not match piece geometry"))
    (with-open-file (stream (piece-reference-pathname reference)
                            :direction :io :if-exists :overwrite
                            :element-type '(unsigned-byte 8))
      (file-position stream (+ (piece-reference-offset reference) begin))
      (write-sequence payload stream)
      (finish-output stream))
    (setf (aref (aref (piece-store-received-blocks store) piece) block) 1)
    (if (not (every (lambda (value) (= value 1))
                    (aref (piece-store-received-blocks store) piece)))
        :accepted
        (if (piece-valid-p store piece)
            (progn (setf (aref (piece-store-completed store) piece) 1) :complete)
            (progn (setf (aref (piece-store-completed store) piece) 0)
                   (fill (aref (piece-store-received-blocks store) piece) 0)
                   :corrupt)))))
