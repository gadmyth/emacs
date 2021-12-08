;;; package --- rest-util.el
;;; Commentary:
;;; Code:

(require 's)
(require 'url-cookie)

(defun store-browser-cookies (domain cookie-keys)
  "Get the browser cookies of DOMAIN whoice key's is COOKIE-KEYS."
  (let* ((param (s-join ":" cookie-keys))
         (scriptlets-dir (expand-file-name "scriptlets" +emacs-context-directory+))
         (command (format "python %s %s %s" (expand-file-name "get-browser-cookies.py" scriptlets-dir) domain param))
         (result (shell-command-to-string command)))
    (dolist (cookie-line (s-split "\n" result))
      (when (> (length cookie-line) 0)
        (let* ((cookie-pair (s-split " " cookie-line))
               (cookie-key (car cookie-pair))
               (cookie-value (cadr cookie-pair)))
          (message "cookie:\nkey: %s\nvalue: %s\n\n" cookie-key cookie-value)
          (url-cookie-store cookie-key cookie-value nil domain "/" t))))))

(provide 'rest-util)
;;; rest-util.el ends here
