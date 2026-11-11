;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT

(in-package #:cl-torrent)

(defclass ag-grpc-binary-stream
    (trivial-gray-streams:fundamental-binary-input-stream
     trivial-gray-streams:fundamental-binary-output-stream)
  ((stream :initarg :stream :reader ag-grpc-stream-underlying)
   (send :initarg :send :reader ag-grpc-stream-send-function)
   (receive :initarg :receive :reader ag-grpc-stream-receive-function)
   (encode :initarg :encode :reader ag-grpc-stream-encode-function)
   (decode :initarg :decode :reader ag-grpc-stream-decode-function)
   (close-function :initarg :close :reader ag-grpc-stream-close-function)
   (input :initform #() :accessor ag-grpc-stream-input)
   (input-position :initform 0 :accessor ag-grpc-stream-input-position)
   (output :initform (make-array 4096 :element-type '(unsigned-byte 8)
                                      :adjustable t :fill-pointer 0)
           :reader ag-grpc-stream-output)
   (closed-p :initform nil :accessor ag-grpc-stream-closed-p)))

(defun default-ag-grpc-receive (stream)
  (typecase stream
    (ag-grpc:grpc-bidi-stream (ag-grpc:stream-read-message stream))
    (ag-grpc:grpc-server-call-stream (ag-grpc:stream-recv stream))
    (t (error "Not an ag-gRPC bidirectional stream: ~S" stream))))

(defun default-ag-grpc-close (stream)
  (when (typep stream 'ag-grpc:grpc-bidi-stream)
    (ag-grpc:stream-close-send stream)))

(defun make-ag-grpc-stream
    (&key stream (send #'ag-grpc:stream-send)
                 (receive #'default-ag-grpc-receive)
                 (encode #'identity) (decode #'identity)
                 (close #'default-ag-grpc-close))
  "Expose an ag-gRPC bidi message stream as a binary Common Lisp stream.

ENCODE turns an octet chunk into the application's generated protobuf message;
DECODE extracts octets from an incoming message.  Identity callbacks work when
ag-gRPC byte-vector messages are used directly."
  (make-instance 'ag-grpc-binary-stream
                 :stream stream :send send :receive receive
                 :encode encode :decode decode :close close))

(defun flush-ag-grpc-output (stream)
  (let ((output (ag-grpc-stream-output stream)))
    (when (plusp (length output))
      (let ((bytes (copy-seq output)))
        (setf (fill-pointer output) 0)
        (funcall (ag-grpc-stream-send-function stream)
                 (ag-grpc-stream-underlying stream)
                 (funcall (ag-grpc-stream-encode-function stream) bytes)))))
  stream)

(defun fill-ag-grpc-input (stream)
  (loop
    for message = (funcall (ag-grpc-stream-receive-function stream)
                           (ag-grpc-stream-underlying stream))
    do (when (null message) (return nil))
       (let ((bytes
               (funcall (ag-grpc-stream-decode-function stream) message)))
         (unless (and (vectorp bytes)
                      (every (lambda (byte) (typep byte '(unsigned-byte 8)))
                             bytes))
           (error "An ag-gRPC peer message did not decode to octets"))
         (when (plusp (length bytes))
           (setf (ag-grpc-stream-input stream)
                 (coerce bytes '(vector (unsigned-byte 8)))
                 (ag-grpc-stream-input-position stream) 0)
           (return t)))))

(defmethod trivial-gray-streams:stream-read-byte
    ((stream ag-grpc-binary-stream))
  (when (>= (ag-grpc-stream-input-position stream)
            (length (ag-grpc-stream-input stream)))
    (unless (fill-ag-grpc-input stream)
      (return-from trivial-gray-streams:stream-read-byte :eof)))
  (prog1 (aref (ag-grpc-stream-input stream)
               (ag-grpc-stream-input-position stream))
    (incf (ag-grpc-stream-input-position stream))))

(defmethod trivial-gray-streams:stream-read-sequence
    ((stream ag-grpc-binary-stream) sequence start end &key)
  (loop with position = start
        while (< position end)
        for byte = (trivial-gray-streams:stream-read-byte stream)
        until (eq byte :eof)
        do (setf (elt sequence position) byte)
           (incf position)
        finally (return position)))

(defmethod trivial-gray-streams:stream-write-byte
    ((stream ag-grpc-binary-stream) byte)
  (unless (typep byte '(unsigned-byte 8))
    (error "A gRPC peer stream accepts only octets"))
  (vector-push-extend byte (ag-grpc-stream-output stream))
  byte)

(defmethod trivial-gray-streams:stream-write-sequence
    ((stream ag-grpc-binary-stream) sequence start end &key)
  (loop for position from start below end
        do (trivial-gray-streams:stream-write-byte stream
                                                  (elt sequence position)))
  sequence)

(defmethod trivial-gray-streams:stream-force-output
    ((stream ag-grpc-binary-stream))
  (flush-ag-grpc-output stream)
  nil)

(defmethod trivial-gray-streams:stream-finish-output
    ((stream ag-grpc-binary-stream))
  (flush-ag-grpc-output stream)
  nil)

(defmethod close ((stream ag-grpc-binary-stream) &key abort)
  (declare (ignore abort))
  (unless (ag-grpc-stream-closed-p stream)
    (unless (zerop (length (ag-grpc-stream-output stream)))
      (flush-ag-grpc-output stream))
    (setf (ag-grpc-stream-closed-p stream) t)
    (funcall (ag-grpc-stream-close-function stream)
             (ag-grpc-stream-underlying stream)))
  t)

(defclass ag-grpc-transport ()
  ((connector :initarg :connector :reader ag-grpc-transport-connector)
   (listener-starter :initarg :listener-starter
                     :reader ag-grpc-transport-listener-starter)
   (listener-stopper :initarg :listener-stopper
                     :reader ag-grpc-transport-listener-stopper)
   (send :initarg :send :reader ag-grpc-transport-send)
   (receive :initarg :receive :reader ag-grpc-transport-receive)
   (encode :initarg :encode :reader ag-grpc-transport-encode)
   (decode :initarg :decode :reader ag-grpc-transport-decode)
   (close :initarg :close :reader ag-grpc-transport-close)))

(defstruct (ag-grpc-peer-listener
             (:include peer-listener)
             (:constructor %make-ag-grpc-peer-listener))
  delegate stop-function)

(defun make-ag-grpc-transport
    (connector &key listener-starter listener-stopper
                    (send #'ag-grpc:stream-send)
                    (receive #'default-ag-grpc-receive)
                    (encode #'identity) (decode #'identity)
                    (close #'default-ag-grpc-close))
  "Build a peer transport over application-created ag-gRPC bidi streams.

CONNECTOR receives a PEER-ENDPOINT and returns an ag-gRPC stream.  A listener
adapter is optional: LISTENER-STARTER receives a stream handler, host and port,
and returns the application listener plus its bound port as two values."
  (make-instance 'ag-grpc-transport
                 :connector connector :listener-starter listener-starter
                 :listener-stopper listener-stopper :send send :receive receive
                 :encode encode :decode decode :close close))

(defun transport-ag-grpc-stream (transport stream)
  (make-ag-grpc-stream
   :stream stream :send (ag-grpc-transport-send transport)
   :receive (ag-grpc-transport-receive transport)
   :encode (ag-grpc-transport-encode transport)
   :decode (ag-grpc-transport-decode transport)
   :close (ag-grpc-transport-close transport)))

(defmethod open-peer-stream ((transport ag-grpc-transport) endpoint)
  (let* ((grpc-stream
           (funcall (ag-grpc-transport-connector transport) endpoint))
         (binary-stream (transport-ag-grpc-stream transport grpc-stream)))
    (%make-peer-connection
     :stream binary-stream
     :close-function (lambda () (close binary-stream :abort t)))))

(defmethod start-peer-listener ((transport ag-grpc-transport) handler
                                &key (host "0.0.0.0") (port 0))
  (unless (ag-grpc-transport-listener-starter transport)
    (error "This ag-gRPC transport has no listener adapter"))
  (multiple-value-bind (delegate actual-port)
      (funcall
       (ag-grpc-transport-listener-starter transport)
       (lambda (grpc-stream)
         (let* ((binary-stream
                  (transport-ag-grpc-stream transport grpc-stream))
                (connection
                  (%make-peer-connection
                   :stream binary-stream
                   :close-function (lambda () (close binary-stream :abort t)))))
           (unwind-protect (funcall handler connection)
             (close-peer-connection connection))))
       host port)
    (%make-ag-grpc-peer-listener
     :port actual-port :delegate delegate
     :stop-function (ag-grpc-transport-listener-stopper transport))))

(defmethod stop-peer-listener ((listener ag-grpc-peer-listener))
  (when (ag-grpc-peer-listener-stop-function listener)
    (funcall (ag-grpc-peer-listener-stop-function listener)
             (ag-grpc-peer-listener-delegate listener)))
  nil)
