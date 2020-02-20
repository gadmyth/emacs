;;; package --- network-util.el
;;; Commentary:
;;; Code:

(defun current-ip ()
  "."
  (interactive)
  (let* ((ruby-command-string "print Socket.ip_address_list.find { |ai| ai.ipv4? && !ai.ipv4_loopback? }.ip_address")
         (shell-command-string (format "ruby -r socket -e \"%s\"" ruby-command-string))
         (result (shell-command-to-string shell-command-string)))
    (message result)))

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