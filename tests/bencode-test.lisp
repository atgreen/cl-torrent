;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT

(in-package #:cl-torrent/tests)
(in-suite cl-torrent)

(test canonical-bencode-values
  (is (equalp (octets "i-42e") (encode-bencode -42)))
  (is (equalp (octets "4:spam") (encode-bencode "spam")))
  (is (equalp (octets "l4:spami42ee")
              (encode-bencode (list "spam" 42))))
  (let ((dictionary (make-hash-table :test #'equal)))
    (setf (gethash "z" dictionary) 1
          (gethash "a" dictionary) 2)
    (is (equalp (octets "d1:ai2e1:zi1ee")
                (encode-bencode dictionary)))))

(test bencode-round-trip-preserves-binary-strings
  (let* ((binary (make-array 4 :element-type '(unsigned-byte 8)
                             :initial-contents '(0 127 128 255)))
         (value (list binary "text" 7)))
    (is (equalp (list binary (octets "text") 7)
                (decode-bencode (encode-bencode value))))))

(test malformed-and-noncanonical-bencode-is-rejected
  (signals bencode-error (decode-bencode (octets "i03e")))
  (signals bencode-error (decode-bencode (octets "i-0e")))
  (signals bencode-error (decode-bencode (octets "d1:zi1e1:ai2ee")))
  (signals bencode-error (decode-bencode (octets "1:aJUNK"))))
