;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT

(in-package #:cl-torrent/tests)
(in-suite cl-torrent)

(test tracker-announce-query-uses-bytewise-percent-encoding
  (let ((path
          (tracker-announce-path
           "/announce?token=kept"
           (coerce (loop for byte below 20 collect byte)
                   '(vector (unsigned-byte 8)))
           (octets "-CL0001-TRACKERTEST1") 6881
           :uploaded 12 :downloaded 34 :left 56 :event :started)))
    (is (string=
         "/announce?token=kept&info_hash=%00%01%02%03%04%05%06%07%08%09%0A%0B%0C%0D%0E%0F%10%11%12%13&peer_id=-CL0001-TRACKERTEST1&port=6881&uploaded=12&downloaded=34&left=56&compact=1&event=started"
         path))))

(test tracker-response-decodes-compact-ipv4-and-ipv6-fixtures
  (let* ((ipv4 #(127 0 0 1 26 225))
         (ipv6 #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 26 226))
         (response
           (parse-tracker-response
            (encode-bencode
             (bencode-dictionary
              (list (cons "complete" 3)
                    (cons "incomplete" 4)
                    (cons "interval" 1800)
                    (cons "min interval" 900)
                    (cons "peers" ipv4)
                    (cons "peers6" ipv6)
                    (cons "warning message" "fixture warning")))))))
    (is (= 1800 (tracker-response-interval response)))
    (is (= 900 (tracker-response-min-interval response)))
    (is (= 3 (tracker-response-complete response)))
    (is (= 4 (tracker-response-incomplete response)))
    (is (string= "fixture warning" (tracker-response-warning response)))
    (is (equal '("127.0.0.1" "0:0:0:0:0:0:0:1")
               (mapcar #'tracker-peer-host
                       (tracker-response-peers response))))
    (is (equal '(6881 6882)
               (mapcar #'tracker-peer-port
                       (tracker-response-peers response))))))

(test tracker-response-supports-the-original-peer-dictionary-format
  (let* ((peer-id (octets "-CL0001-PEERPEERPEER"))
         (peer-dictionary
           (bencode-dictionary
            (list (cons "ip" "seed.example")
                  (cons "peer id" peer-id)
                  (cons "port" 51413))))
         (body
           (bencode-dictionary
            (list (cons "interval" 60)
                  (cons "peers" (list peer-dictionary)))))
         (response (parse-tracker-response (encode-bencode body))))
    (let ((peer (first (tracker-response-peers response))))
      (is (string= "seed.example" (tracker-peer-host peer)))
      (is (= 51413 (tracker-peer-port peer)))
      (is (equalp peer-id (tracker-peer-peer-id peer))))))

(defun tracker-fixture-header (stream)
  (let ((bytes (make-array 256 :element-type '(unsigned-byte 8)
                               :adjustable t :fill-pointer 0)))
    (loop for byte = (read-byte stream)
          do (vector-push-extend byte bytes)
          until (and (>= (length bytes) 4)
                     (equalp #(13 10 13 10)
                             (subseq bytes (- (length bytes) 4)))))
    (trivial-utf-8:utf-8-bytes-to-string bytes)))

(defun call-tracker-fixture (tls-p)
  (let* ((certificate
           (asdf:system-relative-pathname
            "cl-torrent/tests" "tests/fixtures/localhost.crt"))
         (key
           (asdf:system-relative-pathname
            "cl-torrent/tests" "tests/fixtures/localhost.key"))
         (body
           (encode-bencode
            (bencode-dictionary
             (list (cons "interval" 120)
                   (cons "peers" #(127 0 0 1 26 225))))))
         (listener (usocket:socket-listen
                    "127.0.0.1" 0 :reuse-address t
                    :element-type '(unsigned-byte 8)))
         (port (usocket:get-local-port listener))
         (request nil)
         (server-error nil)
         (thread
           (bt:make-thread
            (lambda ()
              (let ((client nil) (stream nil))
                (unwind-protect
                     (handler-case
                         (progn
                           (setf client
                                 (usocket:socket-accept
                                  listener :element-type '(unsigned-byte 8))
                                 stream (usocket:socket-stream client))
                           (when tls-p
                             (setf stream
                                   (pure-tls:make-tls-server-stream
                                    stream :certificate certificate :key key)))
                           (setf request (tracker-fixture-header stream))
                           (write-sequence
                            (octets
                             (if tls-p
                                 (format nil
                                         "HTTP/1.1 200 OK~C~CContent-Length: ~D~C~CConnection: close~C~C~C~C"
                                         #\Return #\Linefeed (length body)
                                         #\Return #\Linefeed #\Return #\Linefeed
                                         #\Return #\Linefeed)
                                 (format nil
                                         "HTTP/1.1 200 OK~C~CTransfer-Encoding: chunked~C~CConnection: close~C~C~C~C~X~C~C"
                                         #\Return #\Linefeed #\Return #\Linefeed
                                         #\Return #\Linefeed #\Return #\Linefeed
                                         (length body) #\Return #\Linefeed)))
                            stream)
                           (write-sequence body stream)
                           (unless tls-p
                             (write-sequence (octets (format nil "~C~C0~C~C~C~C"
                                                             #\Return #\Linefeed
                                                             #\Return #\Linefeed
                                                             #\Return #\Linefeed))
                                             stream))
                           (finish-output stream))
                       (error (condition) (setf server-error condition)))
                  (when stream (ignore-errors (close stream :abort t)))
                  (when client (ignore-errors (usocket:socket-close client)))
                  (ignore-errors (usocket:socket-close listener)))))
            :name "cl-torrent tracker fixture")))
    (unwind-protect
         (let* ((context
                  (and tls-p
                       (pure-tls:make-tls-context
                        :verify-mode pure-tls:+verify-required+
                        :ca-file certificate)))
                (response
                  (announce-tracker
                   (format nil "~A://127.0.0.1:~D/announce"
                           (if tls-p "https" "http") port)
                   (make-array 32 :element-type '(unsigned-byte 8)
                                  :initial-element 7)
                   (octets "-CL0001-TRACKERTEST1") 6881
                   :left 0 :tls-context context)))
           (bt:join-thread thread)
           (setf thread nil)
           (is (null server-error) "~A" server-error)
           (is (= 120 (tracker-response-interval response)))
           (is (search "GET /announce?info_hash=" request))
           (is (search "&compact=1 HTTP/1.1" request)))
      (when thread
        (ignore-errors (usocket:socket-close listener))
        (bt:join-thread thread)))))

(test tracker-client-interoperates-over-http-and-pure-tls-https
  (call-tracker-fixture nil)
  (call-tracker-fixture t))
