;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT

(in-package #:cl-torrent)

(defclass tcp-transport () ())

(defun make-tcp-transport ()
  (make-instance 'tcp-transport))

(defstruct (tcp-peer-listener
             (:include peer-listener)
             (:constructor %make-tcp-peer-listener))
  socket thread (running-p t))

(defun tcp-connection (socket)
  (%make-peer-connection
   :stream (usocket:socket-stream socket)
   :close-function
   (lambda ()
     (ignore-errors (close (usocket:socket-stream socket) :abort t))
     (ignore-errors (usocket:socket-close socket)))))

(defmethod open-peer-stream ((transport tcp-transport) endpoint)
  (declare (ignore transport))
  (tcp-connection
   (usocket:socket-connect (peer-endpoint-host endpoint)
                           (peer-endpoint-port endpoint)
                           :element-type '(unsigned-byte 8))))

(defun run-tcp-listener (listener handler)
  (loop while (tcp-peer-listener-running-p listener)
        do (handler-case
               (let* ((socket (usocket:socket-accept
                               (tcp-peer-listener-socket listener)
                               :element-type '(unsigned-byte 8)))
                      (connection (tcp-connection socket)))
                 (unwind-protect
                      (when (tcp-peer-listener-running-p listener)
                        (funcall handler connection))
                   (close-peer-connection connection)))
             (error (condition)
               (when (tcp-peer-listener-running-p listener)
                 (warn "TCP peer listener stopped after error: ~A" condition)
                 (setf (tcp-peer-listener-running-p listener) nil))))))

(defmethod start-peer-listener ((transport tcp-transport) handler
                                &key (host "0.0.0.0") (port 0))
  (declare (ignore transport))
  (let* ((socket (usocket:socket-listen host port :reuse-address t
                                             :element-type '(unsigned-byte 8)))
         (listener (%make-tcp-peer-listener
                    :port (usocket:get-local-port socket) :socket socket)))
    (setf (tcp-peer-listener-thread listener)
          (bt:make-thread (lambda () (run-tcp-listener listener handler))
                          :name "cl-torrent TCP peer listener"))
    listener))

(defmethod stop-peer-listener ((listener tcp-peer-listener))
  (when (tcp-peer-listener-running-p listener)
    (setf (tcp-peer-listener-running-p listener) nil)
    ;; Closing a listening descriptor from another thread does not reliably wake
    ;; every implementation's blocking ACCEPT. A loopback connection does.
    (let ((wake (ignore-errors
                  (usocket:socket-connect "127.0.0.1"
                                          (peer-listener-port listener)
                                          :element-type '(unsigned-byte 8)))))
      (when wake (ignore-errors (usocket:socket-close wake)))))
  (let ((thread (tcp-peer-listener-thread listener)))
    (when (and thread (not (eq thread (bt:current-thread))))
      (bt:join-thread thread)))
  (ignore-errors (usocket:socket-close (tcp-peer-listener-socket listener)))
  nil)
