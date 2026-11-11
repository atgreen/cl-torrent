;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT

(asdf:defsystem "cl-torrent"
  :description "A transport-independent BitTorrent v2 library"
  :author "Anthony Green <green@moxielogic.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("ironclad" "trivial-utf-8")
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "bencode")
                             (:file "metainfo")
                             (:file "piece-store")
                             (:file "transport")
                             (:file "peer-wire"))))
  :in-order-to ((test-op (test-op "cl-torrent/tests"))))

(asdf:defsystem "cl-torrent/tcp"
  :description "Standard TCP peer transport for cl-torrent"
  :depends-on ("cl-torrent" "bordeaux-threads" "usocket")
  :components ((:module "src" :components ((:file "transport-tcp")))))

(asdf:defsystem "cl-torrent/tls"
  :description "pure-tls peer transport decorator for cl-torrent"
  :depends-on ("cl-torrent" "pure-tls")
  :components ((:module "src" :components ((:file "transport-tls")))))

(asdf:defsystem "cl-torrent/tracker-http"
  :description "BEP 3 HTTP and HTTPS tracker client for cl-torrent"
  :depends-on ("cl-torrent" "pure-tls" "usocket")
  :components ((:module "src" :components ((:file "tracker-http")))))

(asdf:defsystem "cl-torrent/ag-grpc"
  :description "ag-gRPC message-stream transport adapter for cl-torrent"
  :depends-on ("cl-torrent" "ag-grpc" "trivial-gray-streams")
  :components ((:module "src" :components ((:file "transport-ag-grpc")))))

(asdf:defsystem "cl-torrent/tests"
  :depends-on ("cl-torrent/tcp" "cl-torrent/tls"
               "cl-torrent/tracker-http" "cl-torrent/ag-grpc" "fiveam")
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "bencode-test")
                             (:file "metainfo-test")
                             (:file "piece-store-test")
                             (:file "peer-wire-test")
                             (:file "transport-test")
                             (:file "transport-ag-grpc-test")
                             (:file "tracker-http-test"))))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call :fiveam :run!
                                       (find-symbol "CL-TORRENT" "CL-TORRENT/TESTS"))
               (error "cl-torrent tests failed"))))
