;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT

(in-package #:cl-torrent/tests)
(in-suite cl-torrent)

(test tcp-transport-runs-a-complete-peer-wire-transfer
  (with-test-directory (source-root)
    (with-test-directory (destination-root)
      (let* ((bytes (patterned-octets 40000 17))
             (source (merge-pathnames "payload.bin" source-root))
             (seed-id (octets "-CL0001-SEEDSEEDSEED"))
             (leech-id (octets "-CL0001-LEECHLEECHLE"))
             (transport (make-tcp-transport))
             (listener nil)
             (server-error nil))
        (write-test-file source bytes)
        (let* ((torrent (parse-metainfo
                         (torrent-metainfo
                          (make-torrent source :piece-length 16384))))
               (store (open-piece-store torrent destination-root)))
          (unwind-protect
               (progn
                 (setf listener
                       (start-peer-listener
                        transport
                        (lambda (connection)
                          (handler-case
                              (serve-peer-stream
                               (peer-connection-stream connection)
                               torrent source-root seed-id #*111)
                            (error (condition) (setf server-error condition))))
                        :host "127.0.0.1" :port 0))
                 (let ((connection
                         (open-peer-stream
                          transport
                          (make-peer-endpoint
                           :host "127.0.0.1" :port (peer-listener-port listener)))))
                   (unwind-protect
                        (is-true
                         (download-from-peer-stream
                          (peer-connection-stream connection)
                          torrent store leech-id :pipeline-depth 4))
                     (close-peer-connection connection)))
                 (stop-peer-listener listener)
                 (setf listener nil)
                 (is (null server-error) "~A" server-error)
                 (is (equalp bytes
                             (read-octet-file
                              (merge-pathnames "payload.bin" destination-root)))))
            (when listener (stop-peer-listener listener))))))))

(test tls-transport-decorates-a-peer-transport
  (with-test-directory (source-root)
    (with-test-directory (destination-root)
      (let* ((bytes (patterned-octets 40000 23))
             (source (merge-pathnames "payload.bin" source-root))
             (seed-id (octets "-CL0001-TLSSEEDSEED1"))
             (leech-id (octets "-CL0001-TLSLEECHLEEZ"))
             (certificate
               (asdf:system-relative-pathname
                "cl-torrent/tests" "tests/fixtures/localhost.crt"))
             (key
               (asdf:system-relative-pathname
                "cl-torrent/tests" "tests/fixtures/localhost.key"))
             (transport
               (make-tls-transport
                (make-tcp-transport)
                :hostname "localhost"
                :context (pure-tls:make-tls-context
                          :verify-mode pure-tls:+verify-required+
                          :ca-file certificate)
                :certificate certificate
                :key key
                :alpn-protocols '("bittorrent")))
             (listener nil)
             (server-error nil))
        (write-test-file source bytes)
        (let* ((torrent (parse-metainfo
                         (torrent-metainfo
                          (make-torrent source :piece-length 16384))))
               (store (open-piece-store torrent destination-root)))
          (unwind-protect
               (progn
                 (setf listener
                       (start-peer-listener
                        transport
                        (lambda (connection)
                          (handler-case
                              (serve-peer-stream
                               (peer-connection-stream connection)
                               torrent source-root seed-id #*111)
                            (error (condition) (setf server-error condition))))
                        :host "127.0.0.1" :port 0))
                 (let ((connection
                         (open-peer-stream
                          transport
                          (make-peer-endpoint
                           :host "127.0.0.1"
                           :port (peer-listener-port listener)))))
                   (unwind-protect
                        (is-true
                         (download-from-peer-stream
                          (peer-connection-stream connection)
                          torrent store leech-id :pipeline-depth 4))
                     (close-peer-connection connection)))
                 (stop-peer-listener listener)
                 (setf listener nil)
                 (is (null server-error) "~A" server-error)
                 (is (equalp bytes
                             (read-octet-file
                              (merge-pathnames "payload.bin"
                                               destination-root)))))
            (when listener (stop-peer-listener listener))))))))
