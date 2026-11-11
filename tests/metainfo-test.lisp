;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT

(in-package #:cl-torrent/tests)
(in-suite cl-torrent)

(defun write-test-file (pathname contents)
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname :direction :output
                                   :if-exists :supersede
                                   :element-type '(unsigned-byte 8))
    (write-sequence contents stream)))

(test single-file-v2-metainfo-round-trip
  (with-test-directory (root)
    (let ((source (merge-pathnames "hello.txt" root)))
      (write-test-file source (octets "hello transport-independent torrent"))
      (let* ((torrent (make-torrent source :piece-length 16384
                                           :announce "https://tracker.example/announce"))
             (parsed (parse-metainfo (torrent-metainfo torrent))))
        (is (string= "hello.txt" (torrent-name parsed)))
        (is (= 16384 (torrent-piece-length parsed)))
        (is (string= "https://tracker.example/announce"
                     (torrent-announce parsed)))
        (is (= 1 (length (torrent-files parsed))))
        (is (= 32 (length (torrent-infohash parsed))))
        (is (equalp (torrent-infohash torrent)
                    (torrent-infohash parsed)))))))

(test multi-file-v2-metainfo-has-deterministic-infohash
  (with-test-directory (root)
    (let ((first (merge-pathnames "tree/a.txt" root))
          (second (merge-pathnames "tree/nested/b.bin" root)))
      (write-test-file first (octets "alpha"))
      (write-test-file second (make-array 20000 :element-type '(unsigned-byte 8)
                                         :initial-element 9))
      (let ((left (make-torrent (list second first) :name "tree" :piece-length 16384))
            (right (make-torrent (list first second) :name "tree" :piece-length 16384)))
        (is (equalp (torrent-infohash left) (torrent-infohash right)))
        (is (= 2 (length (torrent-files left))))))))

(test invalid-v2-piece-length-is-rejected
  (with-test-directory (root)
    (let ((source (merge-pathnames "tiny" root)))
      (write-test-file source (octets "tiny"))
      (signals error (make-torrent source :piece-length 10000)))))
