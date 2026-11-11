;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT

(in-package #:cl-torrent/tests)
(in-suite cl-torrent)

(test standard-peer-handshake
  (let* ((hash (make-array 32 :element-type '(unsigned-byte 8)
                              :initial-contents (loop for i below 32 collect i)))
         (peer (octets "-CL0001-ABCDEFGHIJKL"))
         (wire (encode-peer-handshake hash peer))
         (decoded (decode-peer-handshake wire)))
    (is (= 68 (length wire)))
    (is (equalp (subseq hash 0 20) (peer-handshake-infohash decoded)))
    (is (equalp peer (peer-handshake-peer-id decoded)))))

(test standard-peer-messages-round-trip
  (dolist (message (list (make-peer-message :kind :keepalive)
                         (make-peer-message :kind :unchoke)
                         (make-peer-message :kind :have :piece 7)
                         (make-peer-message :kind :bitfield :payload #(128 1))
                         (make-peer-message :kind :request :piece 2 :begin 16384 :length 4096)
                         (make-peer-message :kind :piece :piece 2 :begin 0 :payload #(1 2 3))))
    (let ((decoded (decode-peer-message (encode-peer-message message))))
      (is (eq (peer-message-kind message) (peer-message-kind decoded)))
      (is (= (peer-message-piece message) (peer-message-piece decoded)))
      (is (= (peer-message-begin message) (peer-message-begin decoded)))
      (is (equalp (peer-message-payload message) (peer-message-payload decoded)))))
  (signals error
    (encode-peer-message (make-peer-message :kind :request :length 16385))))
