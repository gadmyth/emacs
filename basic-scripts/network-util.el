;;; package --- network-util.el
;;; Commentary:
;;; Code:

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
    (let* ((ruby-command-string "print Socket.ip_address_list.find { |ai| ai.ipv4? && !ai.ipv4_loopback? }.ip_address")
           (shell-command-string (format "ruby -r socket -e \"%s\"" ruby-command-string))
           (result (shell-command-to-string shell-command-string)))
      (when current-prefix-arg
        (message "current-ip: %s" result))
      result))
   (t "")))

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
      result))
   (t "")))

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