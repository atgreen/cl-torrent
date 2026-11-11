;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT

(in-package #:cl-torrent/tests)
(in-suite cl-torrent)

(test ag-grpc-message-bridge-behaves-like-a-binary-stream
  (let ((incoming (list #(1 2) #(3 4 5)))
        (outgoing nil)
        (closed nil))
    (let ((stream
            (make-ag-grpc-stream
             :stream :fixture
             :send (lambda (stream message)
                     (is (eq :fixture stream))
                     (push (copy-seq message) outgoing))
             :receive (lambda (stream)
                        (is (eq :fixture stream))
                        (pop incoming))
             :close (lambda (stream)
                      (is (eq :fixture stream))
                      (setf closed t)))))
      (write-sequence #(10 11) stream)
      (write-byte 12 stream)
      (finish-output stream)
      (write-sequence #(13 14) stream)
      (force-output stream)
      (is (equalp '(#(10 11 12) #(13 14)) (nreverse outgoing)))
      (let ((received (make-array 5 :element-type '(unsigned-byte 8))))
        (is (= 5 (read-sequence received stream)))
        (is (equalp #(1 2 3 4 5) received))
        (is (eq :eof (read-byte stream nil :eof))))
      (close stream)
      (is-true closed))))

(test ag-grpc-transport-satisfies-the-peer-connection-contract
  (let ((endpoint-seen nil)
        (closed nil))
    (let* ((transport
             (make-ag-grpc-transport
              (lambda (endpoint)
                (setf endpoint-seen endpoint)
                :grpc-stream)
              :send (lambda (stream message)
                      (declare (ignore stream message)))
              :receive (lambda (stream)
                         (declare (ignore stream)) nil)
              :close (lambda (stream)
                       (is (eq :grpc-stream stream))
                       (setf closed t))))
           (endpoint (make-peer-endpoint :host "peer.example" :port 443))
           (connection (open-peer-stream transport endpoint)))
      (is (eq endpoint endpoint-seen))
      (is (streamp (peer-connection-stream connection)))
      (close-peer-connection connection)
      (close-peer-connection connection)
      (is-true closed))))
