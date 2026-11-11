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

(test blob-torrent-is-named-by-content-not-its-scratch-path
  ;; A content-addressed blob is staged in a scratch file whose on-disk name is
  ;; irrelevant; the torrent must identify it by its content id, in both the info
  ;; name and the single file-tree entry, and survive a metainfo round-trip.
  (with-test-directory (root)
    (let ((source (merge-pathnames "scratch-0a1b2c.tmp" root)))
      (write-test-file source (make-array 40000 :element-type '(unsigned-byte 8)
                                                 :initial-element 7))
      (let* ((torrent (make-blob-torrent source :piece-length 16384
                                                :content-id "sha256:feedface"))
             (parsed (parse-blob-metainfo (torrent-metainfo torrent))))
        (is (blob-torrent-p torrent))
        (is (string= "sha256:feedface" (torrent-name torrent)))
        (is (string= "sha256:feedface" (blob-torrent-content-id parsed)))
        (is (equalp '("sha256:feedface")
                    (cl-torrent::torrent-file-path (blob-torrent-file parsed))))
        (is (= 40000 (blob-torrent-length parsed)))
        (is (equalp (torrent-metainfo torrent) (torrent-metainfo parsed)))))))

(test blob-torrent-name-overrides-the-content-id
  ;; An explicit name wins over the content id for both the torrent name and the
  ;; in-tree filename, while the content id is still carried for binding.
  (with-test-directory (root)
    (let ((source (merge-pathnames "scratch.tmp" root)))
      (write-test-file source (octets "small blob"))
      (let ((torrent (make-blob-torrent source :piece-length 16384
                                               :content-id "sha256:abc123"
                                               :name "deadbeef")))
        (is (string= "deadbeef" (torrent-name torrent)))
        (is (equalp '("deadbeef") (cl-torrent::torrent-file-path (blob-torrent-file torrent))))
        (is (string= "sha256:abc123" (blob-torrent-content-id torrent)))))))
