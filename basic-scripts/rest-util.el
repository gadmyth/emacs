;;; package --- rest-util.el
;;; Commentary:
;;; Code:

(require 'url-cookie)

(defun refresh-cookies (domain cookie-keys)
  "Get the browser cookies of DOMAIN whoice key's is COOKIE-KEYS."
  (url-cookie-delete-cookies domain)
  (let* ((param (mapconcat 'identity cookie-keys ":"))
         (scriptlets-dir (expand-file-name "scriptlets" +emacs-context-directory+))
         (python-command (executable-find "python3"))
         (script-path (expand-file-name "get-browser-cookies.py" scriptlets-dir))
         (command (format "%s %s %s %s" python-command script-path domain param))
         (result (shell-command-to-string command))
         (result (split-string result "\n"))
         (cookie-list (seq-filter (lambda (s) (not (string-empty-p s))) result))
         (cookie-pairs (seq-map (lambda (s) (split-string s)) cookie-list)))
    (dolist (cookie-pair cookie-pairs)
      (let* ((cookie-key (car cookie-pair))
             (cookie-value (cadr cookie-pair)))
        (message "cookie:\nkey: %s\nvalue: %s\n\n" cookie-key cookie-value)
        (url-cookie-store cookie-key cookie-value nil domain "/" t)))))

(provide 'rest-util)
;;; rest-util.el ends here
