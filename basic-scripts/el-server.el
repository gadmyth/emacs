;;; package --- el-server.el
;;; Commentary:
;;; Code:

(require 'elnode)
(require 'mimes)
(require 'network-util)

(defun elnode--ip-host (ip-addr)
  "IP-ADDR: ."
  (destructuring-bind (a b c d port)
      (mapcar 'identity ip-addr)
    (format "%s.%s.%s.%s" a b c d)))

(defun elnode--ip-port (ip-addr)
  "IP-ADDR: ."
  (destructuring-bind (a b c d port)
      (mapcar 'identity ip-addr)
    port))

(defun elnode-remote-host (httpcon)
  "HTTPCON: ."
  (elnode--ip-host
   (plist-get
    (process-contact httpcon t)
    :remote)))

(defun elnode-remote-port (httpcon)
  "HTTPCON: ."
  (elnode--ip-port
   (plist-get
    (process-contact httpcon t)
    :remote)))

(defun elnode-local-host (httpcon)
  "HTTPCON: ."
  (elnode--ip-host
   (plist-get
    (process-contact (process-get httpcon :server) t)
    :local)))

(defun elnode-local-port (httpcon)
  "HTTPCON: ."
  (elnode--ip-port
   (plist-get
    (process-contact (process-get httpcon :server) t)
    :local)))

(setq elnode-error-log-to-messages nil)

(defvar my-default-elnode-url-mapping-table '())

(defun reset-my-default-elnode-url-mapping-table ()
  "."
  (setq my-default-elnode-url-mapping-table '()))

(defvar *my-default-elnode-port* 8000)

(defvar *my-default-elnode-host* "localhost")

(defun my-elnode-add-handlers (handlers)
  "HANDLERS: ."
  (dolist (handler handlers)
    (add-to-list 'my-default-elnode-url-mapping-table handler)))

(defun my-default-elnode-dispatcher-handler (httpcon)
  "HTTPCON: ."
  (elnode-dispatcher httpcon my-default-elnode-url-mapping-table))
(ignore-errors
  (elnode-start 'my-default-elnode-dispatcher-handler :host *my-default-elnode-host* :port *my-default-elnode-port*))

(provide 'el-server)
;;; el-server.el ends here