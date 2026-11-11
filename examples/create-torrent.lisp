;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT

(require :asdf)

(let* ((script (or *load-truename* *compile-file-truename*))
       (root (uiop:pathname-parent-directory-pathname
              (uiop:pathname-directory-pathname script))))
  (asdf:initialize-source-registry
   `(:source-registry
     (:tree ,(merge-pathnames "ocicl/" root))
     (:directory ,root)
     :inherit-configuration))
  (asdf:load-asd (merge-pathnames "cl-torrent.asd" root)))
(asdf:load-system "cl-torrent")

(destructuring-bind (input output &optional announce)
    (uiop:command-line-arguments)
  (let ((torrent (cl-torrent:make-torrent (pathname input) :announce announce)))
    (with-open-file (stream output :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create
                                  :element-type '(unsigned-byte 8))
      (write-sequence (cl-torrent:torrent-metainfo torrent) stream))
    (format t "Wrote ~A (~D-byte v2 infohash).~%"
            output (length (cl-torrent:torrent-infohash torrent)))))
