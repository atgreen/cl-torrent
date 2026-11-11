;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT

(in-package #:cl-torrent)

(defconstant +bep52-block-length+ 16384)

(defstruct torrent-file path length pieces-root piece-hashes source)
(defstruct (torrent (:constructor %make-torrent))
  metainfo infohash name announce piece-length files piece-layers content-id)

(defun sha256 (&rest vectors)
  (ironclad:digest-sequence
   :sha256 (apply #'concatenate '(vector (unsigned-byte 8)) vectors)))

(defun power-of-two-p (number)
  (and (plusp number) (zerop (logand number (1- number)))))

(defun valid-piece-length-p (length)
  (and (integerp length) (>= length +bep52-block-length+)
       (power-of-two-p length)))

(defun read-block-hashes (pathname)
  (with-open-file (stream pathname :direction :input
                                   :element-type '(unsigned-byte 8))
    (let ((buffer (make-array +bep52-block-length+
                              :element-type '(unsigned-byte 8)))
          (result nil))
      (loop for count = (read-sequence buffer stream)
            while (plusp count)
            do (push (ironclad:digest-sequence :sha256 buffer :end count) result))
      (coerce (nreverse result) 'vector))))

(defun zero-hash ()
  (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0))

(defun padded-hashes (hashes minimum)
  (if (zerop (length hashes)) hashes
      (let* ((needed (max minimum (length hashes)))
             (target (ash 1 (integer-length (1- needed))))
             (result (make-array target)))
        (replace result hashes)
        (loop for index from (length hashes) below target
              do (setf (aref result index) (zero-hash)))
        result)))

(defun parent-hashes (hashes)
  (let ((result (make-array (/ (length hashes) 2))))
    (dotimes (index (length result) result)
      (setf (aref result index)
            (sha256 (aref hashes (* 2 index)) (aref hashes (1+ (* 2 index))))))))

(defun file-merkle-tree (pathname piece-length)
  (let* ((length (with-open-file (stream pathname :direction :input
                                                  :element-type '(unsigned-byte 8))
                   (file-length stream)))
         (leaves-per-piece (/ piece-length +bep52-block-length+))
         (level (padded-hashes (read-block-hashes pathname) leaves-per-piece)))
    (if (zerop length)
        (values length nil #())
        (progn
          (loop repeat (integer-length (1- leaves-per-piece)) do
            (setf level (parent-hashes level)))
          (let ((pieces (subseq level 0 (ceiling length piece-length))))
            (loop while (> (length level) 1) do (setf level (parent-hashes level)))
            (values length (aref level 0) pieces))))))

(defun pathname-components (pathname root)
  (let* ((relative (enough-namestring pathname root))
         (parts (uiop:split-string relative :separator '(#\/))))
    (remove-if (lambda (part) (or (string= part "") (string= part "."))) parts)))

(defun common-root (pathnames name)
  (or (and name
           (let* ((first (first pathnames))
                  (directory (uiop:pathname-directory-pathname first))
                  (parent (uiop:pathname-parent-directory-pathname directory)))
             (if (string= (car (last (pathname-directory directory))) name)
                 directory parent)))
      (uiop:pathname-directory-pathname (first pathnames))))

(defun make-file-record (pathname root piece-length single-p &optional filename)
  (multiple-value-bind (length root-hash pieces) (file-merkle-tree pathname piece-length)
    (make-torrent-file
     :path (cond (filename (list filename))
                 (single-p (list (file-namestring pathname)))
                 (t (pathname-components pathname root)))
     :length length :pieces-root root-hash :piece-hashes pieces :source pathname)))

(defun file-properties (file)
  (bencode-dictionary
   (append (list (cons "length" (torrent-file-length file)))
           (when (torrent-file-pieces-root file)
             (list (cons "pieces root" (torrent-file-pieces-root file)))))))

(defun insert-file-tree (tree components properties)
  (let ((name (first components)))
    (if (rest components)
        (let ((child (or (cdr (assoc name tree :test #'string=)) nil)))
          (unless child
            (setf child nil)
            (push (cons name child) tree))
          (let ((entry (assoc name tree :test #'string=)))
            (setf (cdr entry) (insert-file-tree (cdr entry) (rest components) properties)))
          tree)
        (progn
          (push (cons name (list (cons #() properties))) tree)
          tree))))

(defun tree-to-bencode (tree)
  (bencode-dictionary
   (mapcar (lambda (entry)
             (cons (car entry)
                   (if (and (= 1 (length (cdr entry)))
                            (vectorp (caar (cdr entry))))
                       (bencode-dictionary (cdr entry))
                       (tree-to-bencode (cdr entry)))))
           tree)))

(defun make-file-tree (files)
  (let ((root (make-hash-table :test #'equalp)))
    (dolist (file files root)
      (let ((node root))
        (dolist (component (torrent-file-path file))
          (let ((child (or (gethash component node)
                           (setf (gethash component node)
                                 (make-hash-table :test #'equalp)))))
            (setf node child)))
        (setf (gethash #() node) (file-properties file))))))

(defun joined-hashes (hashes)
  (apply #'concatenate '(vector (unsigned-byte 8)) (coerce hashes 'list)))

(defun make-torrent (input &key name (piece-length 262144) announce content-id filename)
  "Create canonical BitTorrent v2 metainfo for INPUT, a pathname or pathname list.
CONTENT-ID, when given, is an opaque external identifier for the content (such as
an OCI digest).  It is stored in the info dictionary -- so it is covered by the
infohash -- binding the swarm's identity to the content's identity.  FILENAME,
for a single-file torrent, is the name the file carries inside the torrent's file
tree, decoupled from the on-disk pathname -- so a content-addressed blob held in a
temporary file can be named by its content identity."
  (unless (valid-piece-length-p piece-length)
    (error "Piece length must be a power of two and at least 16384"))
  (let* ((pathnames (mapcar #'pathname (if (listp input) input (list input))))
         (single-p (= 1 (length pathnames)))
         (name (or name (if single-p (file-namestring (first pathnames)) "torrent")))
         (root (common-root pathnames (and (not single-p) name)))
         (files (sort (mapcar (lambda (path)
                                (make-file-record path root piece-length single-p
                                                  (and single-p filename)))
                              pathnames)
                      #'string< :key (lambda (file)
                                       (format nil "~{~A~^/~}" (torrent-file-path file)))))
         (file-tree (make-file-tree files)))
    (let* (
           (info (bencode-dictionary
                  (append (when content-id (list (cons "content id" content-id)))
                          (list (cons "file tree" file-tree) (cons "meta version" 2)
                                (cons "name" name) (cons "piece length" piece-length)))))
           (layers (bencode-dictionary
                    (loop for file in files
                          when (> (torrent-file-length file) piece-length)
                            collect (cons (torrent-file-pieces-root file)
                                          (joined-hashes (torrent-file-piece-hashes file))))))
           (root-map (bencode-dictionary
                      (append (when announce (list (cons "announce" announce)))
                              (list (cons "info" info) (cons "piece layers" layers)))))
           (metainfo (encode-bencode root-map)))
      (%make-torrent :metainfo metainfo :infohash (sha256 (encode-bencode info))
                    :name name :announce announce :piece-length piece-length
                    :files files :piece-layers layers
                    :content-id (and content-id (utf8 (as-octets content-id)))))))

(defun utf8 (bytes) (trivial-utf-8:utf-8-bytes-to-string bytes))

(defun split-piece-hashes (octets count)
  (unless (and octets (= (length octets) (* count 32)))
    (error "BEP 52 piece layer has the wrong length"))
  (let ((result (make-array count)))
    (dotimes (index count result)
      (setf (aref result index)
            (subseq octets (* index 32) (* (1+ index) 32))))))

(defun populate-piece-hashes (files layers piece-length)
  (dolist (file files files)
    (let ((count (ceiling (torrent-file-length file) piece-length))
          (root (torrent-file-pieces-root file)))
      (setf (torrent-file-piece-hashes file)
            (cond ((zerop count) #())
                  ((= count 1)
                   (unless (and root (= 32 (length root)))
                     (error "BEP 52 file omits its pieces root"))
                   (vector root))
                  (t (split-piece-hashes (bencode-value layers root) count)))))))

(defun collect-file-tree (dictionary &optional prefix)
  (loop for entry in (bencode-map-entries dictionary)
        for key = (car entry) for value = (cdr entry)
        if (zerop (length key))
          collect (make-torrent-file
                   :path prefix :length (bencode-value value "length")
                   :pieces-root (bencode-value value "pieces root"))
        else append (collect-file-tree value (append prefix (list (utf8 key))))))

(defun parse-metainfo (metainfo)
  "Parse and validate canonical BitTorrent v2 METAINFO octets."
  (let* ((root (decode-bencode metainfo))
         (info (bencode-value root "info"))
         (version (and info (bencode-value info "meta version")))
         (piece-length (and info (bencode-value info "piece length")))
         (name (and info (utf8 (bencode-value info "name"))))
         (files (and info (collect-file-tree (bencode-value info "file tree"))))
         (content-bytes (and info (bencode-value info "content id")))
         (layers (bencode-value root "piece layers"))
         (announce-bytes (bencode-value root "announce")))
    (unless (and (equalp metainfo (encode-bencode root)) (= version 2)
                 (valid-piece-length-p piece-length) name files)
      (error "Invalid canonical BitTorrent v2 metainfo"))
    (populate-piece-hashes files layers piece-length)
    (%make-torrent :metainfo (copy-seq metainfo)
                  :infohash (sha256 (encode-bencode info))
                  :name name :announce (and announce-bytes (utf8 announce-bytes))
                  :piece-length piece-length :files files
                  :piece-layers layers
                  :content-id (and content-bytes (utf8 content-bytes)))))

;;; Single-blob torrents: one content-addressed file, optionally bound to an
;;; external content identifier.  These are thin conveniences over the general
;;; engine -- a blob torrent is simply a single-file TORRENT.

(defun make-blob-torrent (blob &key (piece-length 262144) content-id name)
  "Build canonical BEP 52 metainfo for one BLOB (a pathname) as a single-file
torrent.  CONTENT-ID, when given, is an opaque identifier for the blob's content
(such as an OCI digest), carried in the info dictionary and so covered by the
infohash.  The torrent's NAME -- defaulting to the content id, else the blob's
on-disk filename -- is also the name the single file carries inside the file
tree, so a blob staged in a temporary file is identified by its content, not its
scratch path.  Returns a TORRENT with exactly one file."
  (let ((name (or name
                  (and content-id (princ-to-string content-id))
                  (file-namestring (pathname blob)))))
    (make-torrent blob :piece-length piece-length :content-id content-id
                       :name name :filename name)))

(defun blob-torrent-p (object)
  "True when OBJECT is a TORRENT describing exactly one file, so the other
blob-torrent accessors apply."
  (and (torrent-p object) (= 1 (length (torrent-files object)))))

(defun blob-torrent-file (torrent)
  "The single TORRENT-FILE of a blob torrent; an error if TORRENT is multi-file."
  (unless (blob-torrent-p torrent) (error "Not a single-blob torrent"))
  (first (torrent-files torrent)))

(defun blob-torrent-length (torrent)
  "The byte length of the blob."
  (torrent-file-length (blob-torrent-file torrent)))

(defun blob-torrent-content-id (torrent)
  "The external content identifier bound to TORRENT, or NIL when it carries none."
  (torrent-content-id torrent))

(defun parse-blob-metainfo (metainfo)
  "Parse canonical METAINFO octets into a blob torrent, validating that it is
single-file and reconstructing its verified piece geometry.  Signals an error on
malformed or multi-file input.  The bound content id, if any, is available
through BLOB-TORRENT-CONTENT-ID."
  (let ((torrent (parse-metainfo metainfo)))
    (unless (blob-torrent-p torrent)
      (error "Expected a single-blob torrent, found ~D files"
             (length (torrent-files torrent))))
    torrent))
