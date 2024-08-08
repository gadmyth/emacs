;;; package --- rest-util.el
;;; Commentary:
;;; Code:

(require 'url-cookie)

(defun url-retrieve-utf8-json-parser ()
  "."
  (with-current-buffer (current-buffer)
    (let ((content (decode-coding-string (buffer-string) 'utf-8)))
      (json-read-from-string content))))

(defun url-retrieve-utf8-parser ()
  "."
  (with-current-buffer (current-buffer)
    (let ((content (decode-coding-string (buffer-string) 'utf-8)))
      content)))

(defun url-retrieve-utf8-html-parser ()
  "."
  (with-current-buffer (current-buffer)
    (let ((content (decode-coding-string (buffer-string) 'utf-8)))
      (erase-buffer)
      (insert content)
      (libxml-parse-html-region (point-min) (point-max)))))

(defun url-retrieve-utf8-xml-parser ()
  "."
  (with-current-buffer (current-buffer)
    (let ((content (decode-coding-string (buffer-string) 'utf-8)))
      (erase-buffer)
      (insert content)
      (libxml-parse-xml-region (point-min) (point-max)))))

(defun refresh-cookies (domain cookie-keys secure-p)
  "Get the browser cookies of DOMAIN whoice key's is COOKIE-KEYS, SECURE-P."
  (url-cookie-delete-cookies domain)
  (let* ((param (mapconcat 'identity cookie-keys ":"))
         (scriptlets-dir (expand-file-name "scriptlets" +emacs-context-directory+))
         (python-command (executable-find "python3"))
         (script-path (expand-file-name "get-browser-cookies.py" scriptlets-dir))
         (command (format "%s %s %s %s %s" python-command script-path "cookie-pair" domain param))
         (result (shell-command-to-string command))
         (cookie-list (split-string result "\n"))
         (cookie-list (seq-filter (lambda (s) (string-prefix-p "cookie pair:" s)) cookie-list))
         (cookie-pairs (seq-map (lambda (s) (split-string s ":")) cookie-list)))
    (dolist (cookie-pair cookie-pairs)
      (let* ((cookie-key (cadr cookie-pair))
             (cookie-value (caddr cookie-pair)))
        (message "parsed cookie to store: [%s: %s]\n\n" cookie-key cookie-value)
        (url-cookie-store cookie-key cookie-value nil domain "/" secure-p)))))

(defun print-cookies ()
  "Print cookies for a given URL."
  (interactive)
  (let* ((url (read-string "Please input url: "))
         (path (read-string "Please input path: "))
         (secure-p (y-or-n-p "is https? "))
         (cookies (url-cookie-retrieve url path secure-p)))
    (if cookies
        (dolist (cookie cookies)
          (let ((name (url-cookie-name cookie))
                (value (url-cookie-value cookie))
                (domain (url-cookie-domain cookie))
                (path (url-cookie-localpart cookie))
                (secure (url-cookie-secure cookie)))
            (message "Cookie:\n\tName: %s\n\tValue: %s;\n\tDomain: %s;\n\tPath: %s;\n\tSecure: %s"
                     name value domain path (if secure "Yes" "No"))))
      (message "No cookies found for %s" url))))

(provide 'rest-util)
;;; rest-util.el ends here
