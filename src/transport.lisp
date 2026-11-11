;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT

(in-package #:cl-torrent)

(defstruct peer-endpoint host port)
(defstruct (peer-connection (:constructor %make-peer-connection))
  stream close-function (closed-p nil))
(defstruct (peer-listener (:constructor nil)) port)

(defgeneric open-peer-stream (transport endpoint)
  (:documentation "Open a bidirectional binary stream to ENDPOINT."))

(defgeneric start-peer-listener (transport handler &key host port)
  (:documentation "Start accepting peer connections and invoke HANDLER for each one."))

(defgeneric stop-peer-listener (listener)
  (:documentation "Stop LISTENER and wait for its accept loop to finish."))

(defun close-peer-connection (connection)
  (unless (peer-connection-closed-p connection)
    (setf (peer-connection-closed-p connection) t)
    (funcall (peer-connection-close-function connection)))
  nil)
