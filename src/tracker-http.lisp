;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT

(in-package #:cl-torrent)

(define-condition tracker-error (error)
  ((message :initarg :message :reader tracker-error-message))
  (:report (lambda (condition stream)
             (write-string (tracker-error-message condition) stream))))

(defun tracker-fail (control &rest arguments)
  (error 'tracker-error :message (apply #'format nil control arguments)))

(defstruct tracker-peer host port peer-id)
(defstruct tracker-response
  interval min-interval complete incomplete warning peers)
(defstruct tracker-url scheme host port target)

(defun query-octet-safe-p (byte)
  (or (<= (char-code #\A) byte (char-code #\Z))
      (<= (char-code #\a) byte (char-code #\z))
      (<= (char-code #\0) byte (char-code #\9))
      (find byte #(45 46 95 126))))

(defun percent-encode-octets (octets)
  (with-output-to-string (output)
    (loop for byte across (as-octets octets)
          do (if (query-octet-safe-p byte)
                 (write-char (code-char byte) output)
                 (format output "%~2,'0X" byte)))))

(defun tracker-event-name (event)
  (ecase event
    (:started "started") (:completed "completed") (:stopped "stopped")))

(defun tracker-announce-path
    (path infohash peer-id port
     &key (uploaded 0) (downloaded 0) left event ip numwant (compact t))
  "Build a BEP 3 announce target. INFOHASH is truncated to 20 bytes for v2."
  (unless (>= (length infohash) 20)
    (tracker-fail "A tracker infohash must contain at least 20 octets"))
  (unless (= (length peer-id) 20)
    (tracker-fail "A tracker peer id must contain exactly 20 octets"))
  (unless (and (integerp port) (<= 1 port 65535))
    (tracker-fail "A tracker port must be between 1 and 65535"))
  (unless (and (integerp uploaded) (not (minusp uploaded))
               (integerp downloaded) (not (minusp downloaded))
               (integerp left) (not (minusp left)))
    (tracker-fail "Tracker byte counters must be non-negative integers"))
  (let ((separator (if (find #\? path) #\& #\?)))
    (with-output-to-string (output)
      (write-string path output)
      (write-char separator output)
      (format output
              "info_hash=~A&peer_id=~A&port=~D&uploaded=~D&downloaded=~D&left=~D&compact=~D"
              (percent-encode-octets (subseq infohash 0 20))
              (percent-encode-octets peer-id) port uploaded downloaded left
              (if compact 1 0))
      (when event (format output "&event=~A" (tracker-event-name event)))
      (when ip
        (format output "&ip=~A"
                (percent-encode-octets
                 (trivial-utf-8:string-to-utf-8-bytes ip))))
      (when numwant
        (unless (and (integerp numwant) (not (minusp numwant)))
          (tracker-fail "NUMWANT must be a non-negative integer"))
        (format output "&numwant=~D" numwant)))))

(defun octets-text (octets)
  (trivial-utf-8:utf-8-bytes-to-string octets))

(defun compact-ipv4 (bytes start)
  (format nil "~D.~D.~D.~D"
          (aref bytes start) (aref bytes (+ start 1))
          (aref bytes (+ start 2)) (aref bytes (+ start 3))))

(defun compact-ipv6 (bytes start)
  (format nil "~{~X~^:~}"
          (loop for offset from start below (+ start 16) by 2
                collect (+ (ash (aref bytes offset) 8)
                           (aref bytes (1+ offset))))))

(defun compact-peers (bytes address-size)
  (let ((stride (+ address-size 2)))
    (unless (zerop (mod (length bytes) stride))
      (tracker-fail "Malformed compact tracker peer list"))
    (loop for start from 0 below (length bytes) by stride
          for port-start = (+ start address-size)
          for port = (+ (ash (aref bytes port-start) 8)
                        (aref bytes (1+ port-start)))
          when (zerop port) do (tracker-fail "Tracker returned peer port zero")
          collect (make-tracker-peer
                   :host (if (= address-size 4)
                             (compact-ipv4 bytes start)
                             (compact-ipv6 bytes start))
                   :port port))))

(defun dictionary-peers (items)
  (loop for item in items
        for host = (bencode-value item "ip")
        for port = (bencode-value item "port")
        for peer-id = (bencode-value item "peer id")
        do (unless (and host (integerp port) (<= 1 port 65535))
             (tracker-fail "Malformed tracker peer dictionary"))
           (when (and peer-id (/= (length peer-id) 20))
             (tracker-fail "Malformed tracker peer id"))
        collect (make-tracker-peer :host (octets-text host) :port port
                                   :peer-id peer-id)))

(defun optional-tracker-text (dictionary key)
  (let ((value (bencode-value dictionary key)))
    (when value (octets-text value))))

(defun parse-tracker-response (octets)
  "Parse BEP 3, BEP 7 and BEP 23 tracker response OCTETS."
  (let* ((dictionary (decode-bencode octets))
         (failure (optional-tracker-text dictionary "failure reason")))
    (when failure (tracker-fail "Tracker refused announce: ~A" failure))
    (let* ((interval (bencode-value dictionary "interval"))
           (peers (bencode-value dictionary "peers" #()))
           (peers6 (bencode-value dictionary "peers6" #())))
      (unless (and (integerp interval) (plusp interval))
        (tracker-fail "Tracker response has no positive interval"))
      (make-tracker-response
       :interval interval
       :min-interval (bencode-value dictionary "min interval")
       :complete (bencode-value dictionary "complete")
       :incomplete (bencode-value dictionary "incomplete")
       :warning (optional-tracker-text dictionary "warning message")
       :peers (append (if (listp peers)
                          (dictionary-peers peers)
                          (compact-peers peers 4))
                      (compact-peers peers6 16))))))

(defun parse-tracker-url (url)
  (let* ((https (uiop:string-prefix-p "https://" url))
         (http (uiop:string-prefix-p "http://" url)))
    (unless (or https http)
      (tracker-fail "Tracker URL must use http or https"))
    (let* ((scheme (if https :https :http))
           (start (if https 8 7))
           (slash (or (position #\/ url :start start) (length url)))
           (authority (subseq url start slash))
           (target (if (< slash (length url)) (subseq url slash) "/"))
           (bracketed (and (plusp (length authority))
                           (char= #\[ (char authority 0))))
           (close (and bracketed (position #\] authority)))
           (colon (if bracketed
                      (and close (< (1+ close) (length authority))
                           (= (char-code #\:) (char-code (char authority (1+ close))))
                           (1+ close))
                      (position #\: authority :from-end t)))
           (host (cond (bracketed
                        (unless close (tracker-fail "Malformed IPv6 tracker URL"))
                        (subseq authority 1 close))
                       (colon (subseq authority 0 colon))
                       (t authority)))
           (port (if colon
                     (handler-case (parse-integer authority :start (1+ colon))
                       (error () (tracker-fail "Malformed tracker port")))
                     (if https 443 80))))
      (when (or (zerop (length host)) (find #\@ authority))
        (tracker-fail "Malformed tracker authority"))
      (make-tracker-url :scheme scheme :host host :port port :target target))))

(defun write-ascii (stream text)
  (write-sequence (trivial-utf-8:string-to-utf-8-bytes text) stream))

(defun read-http-response (stream)
  (let ((bytes (make-array 4096 :element-type '(unsigned-byte 8)
                                :adjustable t :fill-pointer 0)))
    (loop for byte = (read-byte stream nil nil)
          while byte
          do (when (>= (length bytes) (* 16 1024 1024))
               (tracker-fail "Tracker HTTP response exceeds 16 MiB"))
             (vector-push-extend byte bytes))
    (copy-seq bytes)))

(defun decode-chunked-body (body)
  (let ((position 0)
        (output (make-array 4096 :element-type '(unsigned-byte 8)
                                :adjustable t :fill-pointer 0)))
    (loop
      (let ((line-end (search #(13 10) body :start2 position)))
        (unless line-end (tracker-fail "Malformed HTTP chunk size"))
        (let* ((line (octets-text (subseq body position line-end)))
               (extension (position #\; line))
               (token (string-trim '(#\Space #\Tab)
                                   (subseq line 0 extension)))
               (size (handler-case (parse-integer token :radix 16)
                       (error () (tracker-fail "Malformed HTTP chunk size")))))
          (setf position (+ line-end 2))
          (when (zerop size) (return (copy-seq output)))
          (when (> (+ position size 2) (length body))
            (tracker-fail "Truncated HTTP chunk"))
          (push-octets (subseq body position (+ position size)) output)
          (incf position size)
          (unless (and (= 13 (aref body position))
                       (= 10 (aref body (1+ position))))
            (tracker-fail "Malformed HTTP chunk terminator"))
          (incf position 2))))))

(defun http-body (response)
  (let ((boundary
          (search #(13 10 13 10) response)))
    (unless boundary (tracker-fail "Malformed tracker HTTP response"))
    (let* ((header (octets-text (subseq response 0 boundary)))
           (line-end (search (format nil "~C~C" #\Return #\Linefeed) header))
           (status-line (subseq header 0 line-end))
           (first-space (position #\Space status-line))
           (status (and first-space
                        (parse-integer status-line :start (1+ first-space)
                                                   :junk-allowed t))))
      (unless (and status (<= 200 status 299))
        (tracker-fail "Tracker HTTP status was ~A" (or status "invalid")))
      (let ((body (subseq response (+ boundary 4))))
        (if (search "transfer-encoding: chunked" (string-downcase header))
            (decode-chunked-body body)
            body)))))

(defun tracker-http-request (url &key tls-context)
  (let* ((parsed (parse-tracker-url url))
         (socket (usocket:socket-connect
                  (tracker-url-host parsed) (tracker-url-port parsed)
                  :element-type '(unsigned-byte 8)))
         (raw (usocket:socket-stream socket))
         (stream raw))
    (unwind-protect
         (progn
           (when (eq :https (tracker-url-scheme parsed))
             (setf stream
                   (if tls-context
                       (pure-tls:make-tls-client-stream
                        raw :hostname (tracker-url-host parsed)
                            :context tls-context
                            :verify pure-tls:+verify-required+)
                       (pure-tls:make-tls-client-stream
                        raw :hostname (tracker-url-host parsed)
                            :verify pure-tls:+verify-required+))))
           (write-ascii
            stream
            (format nil "GET ~A HTTP/1.1~C~CHost: ~A~C~CUser-Agent: cl-torrent/0.1~C~CAccept: text/plain~C~CConnection: close~C~C~C~C"
                    (tracker-url-target parsed) #\Return #\Linefeed
                    (if (or (and (eq :http (tracker-url-scheme parsed))
                                 (= 80 (tracker-url-port parsed)))
                            (and (eq :https (tracker-url-scheme parsed))
                                 (= 443 (tracker-url-port parsed))))
                        (tracker-url-host parsed)
                        (format nil "~A:~D" (tracker-url-host parsed)
                                             (tracker-url-port parsed)))
                    #\Return #\Linefeed
                    #\Return #\Linefeed #\Return #\Linefeed
                    #\Return #\Linefeed #\Return #\Linefeed))
           (finish-output stream)
           (http-body (read-http-response stream)))
      (unless (eq stream raw) (ignore-errors (close stream :abort t)))
      (ignore-errors (close raw :abort t))
      (ignore-errors (usocket:socket-close socket)))))

(defun announce-tracker
    (url infohash peer-id port
     &key (uploaded 0) (downloaded 0) left event ip numwant (compact t)
          tls-context)
  "Announce to an HTTP(S) tracker and return a TRACKER-RESPONSE."
  (let* ((parsed (parse-tracker-url url))
         (target (tracker-announce-path
                  (tracker-url-target parsed) infohash peer-id port
                  :uploaded uploaded :downloaded downloaded :left left
                  :event event :ip ip :numwant numwant :compact compact))
         (authority (if (or (and (eq :http (tracker-url-scheme parsed))
                                 (= 80 (tracker-url-port parsed)))
                            (and (eq :https (tracker-url-scheme parsed))
                                 (= 443 (tracker-url-port parsed))))
                        (tracker-url-host parsed)
                        (format nil "~A:~D" (tracker-url-host parsed)
                                             (tracker-url-port parsed))))
         (request-url (format nil "~(~A~)://~A~A"
                              (tracker-url-scheme parsed) authority target)))
    (parse-tracker-response
     (tracker-http-request request-url :tls-context tls-context))))
