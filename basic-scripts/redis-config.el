;;; package --- redis-config.el
;;; Commentary:
;;; Code:

(require 'eredis)

(defun r-connect-local ()
  "."
  (interactive)
  (unless eredis--current-process
    (eredis-connect "localhost" 6379)))

(defun r-connect-server ()
  "."
  (interactive)
  (unless eredis--current-process
    (let ((host (read-string "Please input redis host (localhost as default): " nil nil "localhost"))
          (port (read-number "Please input redis port (6379 as default): " 6379)))
      (eredis-connect host port))))

(defun r-get (key &optional debug)
  "KEY."
  (interactive "sget redis value for key: ")
  (let ((value (eredis-get key)))
    (when debug
      (message value))
    value))

(defun r-set (key value)
  "KEY: VALUE:."
  (interactive"sKey: \nsValue: \n")
  (eredis-set key value))

(defun r-keys (&optional regexp)
  "Get all the keys match the REGEXP if exists."
  (interactive)
  (eredis-keys (or regexp "*")))

(defun r-del (key)
  "KEY: ."
  (interactive)
  (eredis-del key))

(defun r-save ()
  "."
  (interactive)
  (eredis-save))

(provide 'redis-config)
;;; redis-config.el ends here
