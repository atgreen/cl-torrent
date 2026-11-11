;;;; SPDX-FileCopyrightText: 2026 Anthony Green <green@moxielogic.com>
;;;; SPDX-License-Identifier: MIT

(in-package #:cl-torrent)

(defclass tls-transport ()
  ((transport :initarg :transport :reader tls-transport-underlying)
   (hostname :initarg :hostname :reader tls-transport-hostname)
   (verify :initarg :verify :reader tls-transport-verify)
   (context :initarg :context :reader tls-transport-context)
   (client-certificate :initarg :client-certificate
                       :reader tls-transport-client-certificate)
   (client-key :initarg :client-key :reader tls-transport-client-key)
   (certificate :initarg :certificate :reader tls-transport-certificate)
   (key :initarg :key :reader tls-transport-key)
   (server-verify :initarg :server-verify :reader tls-transport-server-verify)
   (trust-store :initarg :trust-store :reader tls-transport-trust-store)
   (alpn-protocols :initarg :alpn-protocols
                   :reader tls-transport-alpn-protocols)))

(defun make-tls-transport
    (transport &key hostname
                 (verify pure-tls:+verify-required+) context
                 client-certificate client-key certificate key
                 (server-verify pure-tls:+verify-none+) trust-store
                 alpn-protocols)
  "Decorate TRANSPORT with TLS using credentials supplied by the application.

HOSTNAME overrides endpoint-host for client certificate verification.  A
listener requires CERTIFICATE and KEY.  CLIENT-CERTIFICATE, CLIENT-KEY,
SERVER-VERIFY and TRUST-STORE enable mutual TLS without coupling cl-torrent to
an identity or PKI implementation."
  (unless (eql (null certificate) (null key))
    (error "TLS server certificate and key must be supplied together"))
  (unless (eql (null client-certificate) (null client-key))
    (error "TLS client certificate and key must be supplied together"))
  (make-instance 'tls-transport
                 :transport transport :hostname hostname :verify verify
                 :context context :client-certificate client-certificate
                 :client-key client-key :certificate certificate :key key
                 :server-verify server-verify :trust-store trust-store
                 :alpn-protocols alpn-protocols))

(defun tls-client-stream (transport stream endpoint)
  (apply #'pure-tls:make-tls-client-stream stream
         (append
          (list :hostname (or (tls-transport-hostname transport)
                              (peer-endpoint-host endpoint))
                :verify (tls-transport-verify transport))
          (when (tls-transport-context transport)
            (list :context (tls-transport-context transport)))
          (when (tls-transport-client-certificate transport)
            (list :client-certificate
                  (tls-transport-client-certificate transport)
                  :client-key (tls-transport-client-key transport)))
          (when (tls-transport-alpn-protocols transport)
            (list :alpn-protocols
                  (tls-transport-alpn-protocols transport))))))

(defun tls-server-stream (transport stream)
  (unless (and (tls-transport-certificate transport)
               (tls-transport-key transport))
    (error "A TLS listener requires a certificate and key"))
  (apply #'pure-tls:make-tls-server-stream stream
         (append
          (list :certificate (tls-transport-certificate transport)
                :key (tls-transport-key transport)
                :verify (tls-transport-server-verify transport))
          (when (tls-transport-context transport)
            (list :context (tls-transport-context transport)))
          (when (tls-transport-trust-store transport)
            (list :trust-store (tls-transport-trust-store transport)))
          (when (tls-transport-alpn-protocols transport)
            (list :alpn-protocols
                  (tls-transport-alpn-protocols transport))))))

(defun wrap-tls-connection (raw stream)
  (%make-peer-connection
   :stream stream
   :close-function
   (lambda ()
     (ignore-errors (close stream :abort t))
     (close-peer-connection raw))))

(defmethod open-peer-stream ((transport tls-transport) endpoint)
  (let ((raw (open-peer-stream (tls-transport-underlying transport) endpoint)))
    (handler-case
        (wrap-tls-connection
         raw (tls-client-stream transport (peer-connection-stream raw) endpoint))
      (error (condition)
        (close-peer-connection raw)
        (error condition)))))

(defmethod start-peer-listener ((transport tls-transport) handler
                                &key (host "0.0.0.0") (port 0))
  (start-peer-listener
   (tls-transport-underlying transport)
   (lambda (raw)
     (let ((connection nil))
       (unwind-protect
            (progn
              (setf connection
                    (wrap-tls-connection
                     raw (tls-server-stream
                          transport (peer-connection-stream raw))))
              (funcall handler connection))
         (when connection (close-peer-connection connection)))))
   :host host :port port))
