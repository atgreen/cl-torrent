;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT

(in-package #:cl-torrent/tests)
(in-suite cl-torrent)

(defun patterned-octets (length multiplier)
  (let ((result (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (index length result)
      (setf (aref result index) (mod (* index multiplier) 251)))))

(defun read-octet-file (pathname)
  (with-open-file (stream pathname :direction :input
                                   :element-type '(unsigned-byte 8))
    (let ((result (make-array (file-length stream)
                              :element-type '(unsigned-byte 8))))
      (read-sequence result stream)
      result)))

(test multi-file-store-verifies-each-files-piece-tree
  (with-test-directory (source-root)
    (with-test-directory (destination-root)
      (let* ((a-bytes (patterned-octets 20000 7))
             (b-bytes (patterned-octets 19000 13))
             (a (merge-pathnames "bundle/a.bin" source-root))
             (b (merge-pathnames "bundle/nested/b.bin" source-root)))
        (write-test-file a a-bytes)
        (write-test-file b b-bytes)
        (let* ((created (make-torrent (list b a) :name "bundle" :piece-length 16384))
               (torrent (parse-metainfo (torrent-metainfo created)))
               (store (open-piece-store torrent destination-root)))
          (is (= 4 (length (completed-pieces store))))
          (is (eq :complete (accept-block store 0 0 (subseq a-bytes 0 16384))))
          (is (eq :complete (accept-block store 1 0 (subseq a-bytes 16384))))
          (let ((corrupt (copy-seq (subseq b-bytes 0 16384))))
            (setf (aref corrupt 0) (logxor #xff (aref corrupt 0)))
            (is (eq :corrupt (accept-block store 2 0 corrupt))))
          (is (eq :complete (accept-block store 2 0 (subseq b-bytes 0 16384))))
          (is (eq :complete (accept-block store 3 0 (subseq b-bytes 16384))))
          (is-true (piece-store-complete-p store))
          (is (equalp a-bytes (read-octet-file (merge-pathnames "a.bin" destination-root))))
          (is (equalp b-bytes (read-octet-file (merge-pathnames "nested/b.bin" destination-root))))
          (let ((resumed (open-piece-store torrent destination-root :resume-p t)))
            (is-true (piece-store-complete-p resumed))))))))

(test piece-store-rejects-misaligned-and-cross-boundary-blocks
  (with-test-directory (source-root)
    (with-test-directory (destination-root)
      (let* ((bytes (patterned-octets 20000 3))
             (source (merge-pathnames "one.bin" source-root)))
        (write-test-file source bytes)
        (let ((store (open-piece-store
                      (parse-metainfo
                       (torrent-metainfo
                        (make-torrent source :piece-length 16384)))
                      destination-root)))
          (signals error (accept-block store 0 1 (subseq bytes 1 20)))
          (signals error (accept-block store 1 0 (make-array 4000 :element-type '(unsigned-byte 8))))
          (signals error (accept-block store 2 0 #())))))))
