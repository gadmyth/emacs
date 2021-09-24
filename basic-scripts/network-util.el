;;; package --- network-util.el
;;; Commentary:
;;; Code:

(defvar *network-util-debug* nil)

(defmacro network-util-debug-message (format-string &rest ARGS)
  "If debug is open, send message with FORMAT-STRING and ARGS."
  `(if *network-util-debug*
       (message ,format-string ,@ARGS)))

(defun network-util-toggle-debug ()
  "."
  (interactive)
  (setq *network-util-debug* (not *network-util-debug*))
  (message "turn %s the %s"
           (if *network-util-debug* "on" "off")
           (symbol-name '*network-util-debug*)))

(defun current-ip ()
  "."
  (interactive)
  (cond
   ((eq window-system 'x)
    (let* ((shell-command-string "ifconfig $(route -n | grep ^0.0.0.0 | awk '{print $NF}') | grep 'inet ' | grep -v 127.0.0.1 | awk '{print $2'}")
           (result (shell-command-to-string shell-command-string))
           (result (substring result 0 (- (length result) 1))))
      (when current-prefix-arg
        (message "current-ip: %s" result))
      result))
   ((eq window-system 'ns)
    (let* ((shell-command-string "ifconfig $(route -n get default | grep interface | awk '{print $2}' | head -n 1) | grep 'inet ' | grep -v 127.0.0.1 | awk '{print $2'}")
           (result (shell-command-to-string shell-command-string))
           (result (substring result 0 (- (length result) 1))))
      (when current-prefix-arg
        (message "current-ip: %s" result))
      result))
   (t "")))

(defvar *fetched-public-ip* "")

(defun public-ip ()
  "."
  (interactive)
  (cond
   ((or (eq window-system 'x)
        (eq window-system 'ns))
    (let* ((shell-command-string "curl -s cip.cc | grep IP | cut -f2 -d ':' | awk '{ gsub(/^[ \t]+|[ \t]+$/, \"\"); print }'")
           (result (shell-command-to-string shell-command-string))
           (result (substring result 0 (- (length result) 1))))
      (when current-prefix-arg
        (message "public-ip: %s" result))
      (when result
        (network-util-debug-message "public ip fetched: [%S]" result)
        (setq *fetched-public-ip* result)
        (network-util-debug-message "*fetched-public-ip* set as: [%S]" *fetched-public-ip*))
      result))
   (t "")))

(defvar *public-ip-fetch-timer* nil)

(defun refresh-public-ip ()
  "."
  (interactive)
  (message "*** now refresh-public-ip: %s ***"
           (timestamp-to-string-with-format (current-timestamp) "%Y-%m-%d %H:%M:%S"))
  (when (> (length *fetched-public-ip*) 0)
    (network-util-debug-message "*fetched-public-ip* is: [%S]" *fetched-public-ip*)
    (setq *fetched-public-ip* "")
    (network-util-debug-message "*fetched-public-ip* set as: [%S]" *fetched-public-ip*))
  (public-ip))

(defun fetched-public-ip ()
  "."
  (when (zerop (length *fetched-public-ip*))
    (when (not *public-ip-fetch-timer*)
      (setq *public-ip-fetch-timer*
            (run-with-timer 1 300 #'refresh-public-ip))))
  *fetched-public-ip*)

(defun reset-public-ip-fetch-timer ()
  "."
  (interactive)
  (when *public-ip-fetch-timer*
    (cancel-timer *public-ip-fetch-timer*)
    (network-util-debug-message "*public-ip-fetch-timer* cancelled")
    (setq *fetched-public-ip* "")
    (network-util-debug-message "*fetched-public-ip* set to [%S]" *fetched-public-ip*)
    (setq *public-ip-fetch-timer* nil)
    (network-util-debug-message "*public-ip-fetch-timer* set to [%S]" *public-ip-fetch-timer*)
    ))

(defun switch-proxy (enable)
  "ENABLE's value is t or nil."
  (interactive "Senable? ")
  (let ((proxy
		 (if enable
			 "127.0.0.1:8087"
		   nil)))
	(setenv "http_proxy"  proxy)
	(setenv "https_proxy" proxy)))

(provide 'network-util)
;;; network-util.el ends here