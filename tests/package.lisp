;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT

(defpackage #:cl-torrent/tests
  (:use #:cl #:fiveam #:cl-torrent))

(in-package #:cl-torrent/tests)

(def-suite cl-torrent)
(in-suite cl-torrent)

(defun octets (text)
  (trivial-utf-8:string-to-utf-8-bytes text))

(defmacro with-test-directory ((variable) &body body)
  `(let ((,variable
           (merge-pathnames
            (format nil "cl-torrent-test-~36R/" (random (expt 36 10)))
            (uiop:temporary-directory))))
     (ensure-directories-exist ,variable)
     (unwind-protect (progn ,@body)
       (uiop:delete-directory-tree ,variable :validate t :if-does-not-exist :ignore))))
