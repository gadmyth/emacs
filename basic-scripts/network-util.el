;;; package --- network-util.el
;;; Commentary:
;;; Code:

(require 'dates)
(require 'q)

(defvar *proxy-list* '((socks5 . "127.0.0.1:1080")
                       (privoxy . "127.0.0.1:8118")))


(define-debug-message network-util)

(defcustom public-ip-fetched-hook nil
  "Hook when public ip fetch finished."
  :type 'hook
  :group 'network-util)

(defun current-ip ()
  "."
  (interactive)
  (cond
   ((eq window-system 'x)
    (let* ((shell-command-string "ifconfig $(route -n | grep ^0.0.0.0 | awk '{print $NF}' | head -n 1) | grep 'inet ' | grep -v 127.0.0.1 | awk '{print $2'}")
           (result (shell-command-to-string shell-command-string)))
      (when (> (length result) 0)
        (setq result (substring result 0 (- (length result) 1))))
      (when (equal current-prefix-arg 17)
        (message "current-ip: %s" result))
      result))
   ((eq window-system 'ns)
    (let* ((shell-command-string "ifconfig $(route -n get default | grep interface | awk '{print $2}' | head -n 1) | grep 'inet ' | grep -v 127.0.0.1 | awk '{print $2'}")
           (result (shell-command-to-string shell-command-string))
           (result (substring result 0 (- (length result) 1))))
      (when (equal current-prefix-arg 17)
        (message "current-ip: %s" result))
      result))
   (t "")))

(defvar *fetched-public-ip* "")

(defun public-ip-bak ()
  "."
  (interactive)
  (cond
   ((or (eq window-system 'x)
        (eq window-system 'ns))
    (cond
     ((eq current-prefix-arg 8)
      (let* ((shell-command-string "curl -s cip.cc | grep IP | cut -f2 -d ':' | awk '{ gsub(/^[ \t]+|[ \t]+$/, \"\"); print }'")
             (result (shell-command-to-string shell-command-string))
             (result (substring result 0 (- (length result) 1))))
        (message "public-ip: %s" result)))
     (t
      (let ((request-backend 'curl))
        (request
          "http://cip.cc"
          :parser 'buffer-string
          :success (cl-function
                    (lambda (&key data &allow-other-keys)
                      (when (> (length data) 0)
                        (let* ((ip-line (car (split-string data "\n")))
                               (ip (string-trim (cadr (split-string ip-line ":")))))
                          (network-util-debug-message "public ip fetched: [%S]" ip)
                          (setq *fetched-public-ip* ip)
                          (network-util-debug-message "*fetched-public-ip* set as: [%S]" *fetched-public-ip*))))))))))
   (t "")))


(defun public-ip ()
  "."
  (interactive)
  (let ((request-backend 'url-retrieve))
    (request
      "http://ifconfig.cc"
      :parser 'buffer-string
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (when (> (length data) 0)
                    (let* ((ip data))
                      (network-util-debug-message "public ip fetched: [%S]" ip)
                      (setq *fetched-public-ip* ip)
                      (run-hooks 'public-ip-fetched-hook)
                      (network-util-debug-message "*fetched-public-ip* set as: [%S]" *fetched-public-ip*))))))))

(defvar *public-ip-fetch-timer* nil)

(defun refresh-public-ip ()
  "."
  (interactive)
  (message "*** now refresh-public-ip: %s ***" (current-time-normal-string))
  (when (> (length *fetched-public-ip*) 0)
    (network-util-debug-message "*fetched-public-ip* is: [%S]" *fetched-public-ip*)
    (setq *fetched-public-ip* "")
    (network-util-debug-message "*fetched-public-ip* set as: [%S]" *fetched-public-ip*))
  (public-ip))

(defun fetched-public-ip ()
  "."
  (when (zerop (length *fetched-public-ip*))
    (when (not *public-ip-fetch-timer*)
      (refresh-public-ip)
      (setq *public-ip-fetch-timer*
            (run-with-idle-timer 300 300 #'refresh-public-ip))))
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

(defun switch-env-proxy ()
  "ENABLE's value is t or nil."
  (interactive)
  (let* ((enable (y-or-n-p "Turn on the env proxy? ")))
    (if enable (turn-on-env-proxy)
      (turn-off-env-proxy))))

(defun choose-proxy ()
  "."
  (let* ((proxy-name (completing-read "Please choose a proxy: " *proxy-list* nil t nil nil 'socks5))
         (proxy (assoc proxy-name *proxy-list* #'string-equal)))
    proxy))

(defun turn-on-env-proxy ()
  "."
  (let ((proxy (choose-proxy)))
    (when proxy
      (do-switch-env-proxy (format "%s://%s" (car proxy) (cdr proxy)))
      (show-env-proxy))))

(defun turn-off-env-proxy ()
  "."
  (do-switch-env-proxy nil))

(defun do-switch-env-proxy (proxy)
  "Switch PROXY."
  (setenv "http_proxy"  proxy)
  (setenv "https_proxy" proxy)
  (setenv "all_proxy"   proxy))

(defun show-env-proxy ()
  "."
  (interactive)
  (let ((http-proxy (getenv "http_proxy"))
        (https-proxy (getenv "https_proxy"))
        (all-proxy (getenv "all_proxy")))
    (message "proxy is:\n\nhttp_proxy: %s\nhttps_proxy: %s\nall_proxy: %s" http-proxy https-proxy all-proxy)))

(defun switch-w3m-proxy ()
  "ENABLE's value is t or nil."
  (interactive)
  (let* ((enable (y-or-n-p "Turn on the w3m proxy? ")))
    (when enable (turn-on-w3m-proxy)
          (turn-off-w3m-proxy)))
  (show-w3m-proxy))

(defun show-w3m-proxy ()
  "."
  (interactive)
  (if (featurep 'w3m)
      (message "w3m proxy: %S" w3m-command-arguments-alist)
    (message "w3m is not installed or not supported!")))

(defun turn-on-w3m-proxy ()
  "."
  (if (featurep 'w3m)
      (let ((proxy (choose-proxy)))
        (setq w3m-command-arguments-alist
              `((""
                 "-o" (format "http_proxy=%s" ,proxy)
                 "-o" (format "https_proxy=%s" ,proxy))))
        (show-url-proxy))
    (message "w3m is not installed or not supported!")))

(defun turn-off-w3m-proxy ()
  "."
  (if (featurep 'w3m)
      (setq w3m-command-arguments-alist nil)
    (message "w3m is not installed or not supported!")))

(defun switch-url-proxy ()
  "ENABLE's value is t or nil."
  (interactive)
  (let* ((enable (y-or-n-p "Turn on the url proxy? ")))
    (if enable (turn-on-url-proxy)
      (turn-off-url-proxy))
    (show-url-proxy)))

;; url package use the url-proxy-services: url-retrieve, url-retrieve-synchronously
(defun show-url-proxy ()
  "."
  (interactive)
  (message "url proxy:\n\nhttp: %S\nhttps: %S\nsocks5: %S"
           (assoc-default "http" url-proxy-services #'string-equal)
           (assoc-default "https" url-proxy-services #'string-equal)
           (assoc-default "socks5" url-proxy-services #'string-equal)))

(defun turn-on-url-proxy ()
  "."
  (let ((proxy (choose-proxy)))
    (when proxy
      (setq url-proxy-services
            (pcase (car proxy)
              ('privoxy
               `(("http" . ,(cdr proxy))
                 ("https" . ,(cdr proxy))))
              ('socks5
               `(("socks5" . ,(cdr proxy)))))))))

(defun turn-off-url-proxy ()
  "."
  (setq url-proxy-services nil))

(provide 'network-util)
;;; network-util.el ends here
