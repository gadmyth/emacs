;;; package --- el-server-extend.el
;;; Commentary:
;;; Code:

(require 'el-server)

(defmacro org-dir-handler-maker (dir)
  "DIR: ."
  "This body must be a macro, because elnode-docroot-for is a macro, the dir should be evaluated."
  `(lambda (httpcon)
     (elnode-docroot-for ,dir
       with org-file
       on httpcon
       do
       (message (format "\nrequest: http://%s%s\n" (elnode-http-header httpcon "Host") (elnode-http-pathinfo httpcon)))
       (let ((remote-host (elnode-remote-host httpcon))
             (remote-port (elnode-remote-port httpcon))
             (local-host (elnode-local-host httpcon))
             (local-port (elnode-local-port httpcon))
             (link-org-as-html (elnode-http-param httpcon "link-org-as-html"))
             (force-refresh (elnode-http-param httpcon "force-refresh"))
             (exclude-inner-res (elnode-http-param httpcon "exclude-inner-res")))
         (message (format "remote host: %s, remote port: %s" remote-host remote-port))
         (message (format "local host: %s, local port: %s" local-host local-port))
         (message (format "org-file: %S, is directory: %S" org-file (file-directory-p org-file)))
         (if (and (equal "0.0.0.0" local-host)
                  (or (equal "0.0.0.0" remote-host)
                      (equal "127.0.0.1" remote-host)
                      (equal "localhost" remote-host)))
             (let ((new-url (format "http://%s:%s%s" (current-ip) local-port (elnode-http-pathinfo httpcon))))
               (message "redirect to new-url: %s" new-url)
               (elnode-send-redirect httpcon new-url))
           (if (not (string-suffix-p "org" org-file))
               (if (file-directory-p org-file)
                   (elnode-send-status httpcon 403 "Permission Denied.")
                 (elnode--webserver-handler-proc httpcon ,dir elnode-webserver-extra-mimetypes))
             (let* ((org-file-mtime (elnode-file-modified-time org-file))
                    (html-file (concat (string-remove-suffix "org" org-file) "html"))
                    (html-file-mtime (elnode-file-modified-time html-file)))
               (if (and org-file-mtime
                        html-file-mtime
                        (time-less-p org-file-mtime html-file-mtime)
                        (not force-refresh))
                   (let* ((path (elnode-http-pathinfo httpcon))
                          (new-path (concat (string-remove-suffix "org" path) "html"))
                          (new-url (format "http://%s:%s%s" remote-host local-port new-path)))
                     (message "rediect org file to html file: %s" new-url)
                     (elnode-send-redirect httpcon new-url))
                 (with-current-buffer (find-file-noselect org-file)
                   (progn
                     (when (string-equal mode-name "not loaded yet")
                       (revert-buffer nil t))
                     (progn
                       (setq org-export-show-temporary-export-buffer nil)
                       (setq-local org-html-link-org-files-as-html link-org-as-html)
                       (setq org-html-head (if exclude-inner-res "" org-html-head-default))
                       (let ((exported-buffer (org-html-export-as-html)))
                         (message "exported-buffer: %s" exported-buffer)
                         (message "org-html-head: %S" (length org-html-head))
                         (setq org-export-show-temporary-export-buffer t)
                         (with-current-buffer exported-buffer
                           (let ((org-html (buffer-substring-no-properties (point-min) (point-max))))
                             (write-file html-file)
                             (elnode-send-html httpcon org-html)))))))))))))))

(defmacro org-dir-compiled-handler-maker (dir)
  "DIR: ."
  (byte-compile `(org-dir-handler-maker ,dir)))


(defun elnode-make-org-webserver (docroot port &optional host)
  "DOCROOT: , PORT: , HOST: . Make a webserver parsing org file to html format."
  (interactive
   (let ((docroot (read-directory-name "Docroot: " nil nil t))
         (port (read-from-minibuffer "Port: "))
         (host (if current-prefix-arg
                   (read-from-minibuffer "Host: ")
                 elnode-init-host)))
     (list docroot port host)))
  (let ((webserver-proc (eval `(org-dir-handler-maker ,docroot))))
    (elnode-start
     webserver-proc
     :port (string-to-number (format "%s" port))
     :host host)
    webserver-proc))

(defun elnode-switch-org ()
  "."
  (lambda (httpcon)
    (require 'redis-config)
    (let* ((org-file-name (elnode-http-param httpcon "name"))
           (result (elnode-switch-org-file org-file-name)))
      (message "elnode-switch-org, file name: %S" org-file-name)
      (elnode-http-start httpcon 200 '("Content-Type" . "text/html"))
      (elnode-http-return httpcon result))))

(defun elnode-switch-org-file (name)
  "NAME."
  (let* ((name (format "%s.org" name))
         (buffer (get-buffer name)))
    (if (buffer-live-p buffer)
        (progn
          (switch-to-buffer buffer)
          (format "switch buffer: %s" name))
      (progn
        (let* ((default-directory (expand-file-name "~/org/"))
               (command (format "git ls-files | egrep \"%s$\"" name))
               (files (split-string (shell-command-to-string command) "\n" t)))
          (when (= (length files) 1)
            (find-file (car files))
            (format "switch to file: %s" (buffer-file-name))))))))

(defun elnode-url-encoder ()
  "."
  (lambda (httpcon)
    (require 'redis-config)
    (let ((content (elnode-http-param httpcon "content")))
      (message "elnode-url-encoder, content: %S" content)
      (elnode-http-start httpcon 200 '("Content-Type" . "text/html"))
      (elnode-http-return httpcon (url-hexify-string content)))))

(defun elnode-url-decoder ()
  "."
  (lambda (httpcon)
    (require 'redis-config)
    (let ((content (elnode-http-param httpcon "content")))
      (message "elnode-url-decoder, content: %S" content)
      (elnode-http-start httpcon 200 '("Content-Type" . "text/html"))
      (elnode-http-return httpcon (decode-coding-string (url-unhex-string content) 'utf-8)))))

(defun elnode-cache-getter ()
  "DOCROOT: , PORT: , HOST: . Make a redis cache server."
  (lambda (httpcon)
    (require 'redis-config)
    (let* ((cache-key (elnode-http-param httpcon "key"))
           (value (r-get cache-key)))
      (message "elnode-cache-server, cache-key: %S" cache-key)
      (elnode-http-start httpcon 200 '("Content-Type" . "text/html"))
      (kill-new value)
      (elnode-http-return httpcon value))))

(defun elnode-cache-setter ()
  "DOCROOT: , PORT: , HOST: . Make a redis cache server."
  (lambda (httpcon)
    (require 'redis-config)
    (let* ((cache-key (elnode-http-param httpcon "key"))
           (value (elnode-http-param httpcon "value")))
      (message "elnode-cache-server, cache-key: %S, value: %S" cache-key value)
      (r-set cache-key value)
      (elnode-http-start httpcon 200 '("Content-Type" . "text/html"))
      (elnode-http-return httpcon (format "set %S to %S" cache-key value)))))

(defun elnode-cache-keys ()
  "DOCROOT: , PORT: , HOST: . Make a redis cache server."
  (lambda (httpcon)
    (require 'redis-config)
    (r-connect-local)
    (message "elnode-cache-server, get keys.")
    (elnode-http-start httpcon 200 '("Content-Type" . "text/html"))
    (elnode-http-return httpcon (format "keys:\n%s" (s-join "\n"(r-keys))))))

(defun elnode-cache-delete ()
  "DOCROOT: , PORT: , HOST: . Make a redis cache server."
  (lambda (httpcon)
    (require 'redis-config)
    (let* ((cache-key (elnode-http-param httpcon "key")))
      (message "elnode-cache-server, delete key: %S" cache-key)
      (elnode-http-start httpcon 200 '("Content-Type" . "text/html"))
      (elnode-http-return httpcon (format "keys: %S" (r-del cache-key))))))

(defun elnode-cache-saver ()
  "DOCROOT: , PORT: , HOST: . Make a redis cache server."
  (lambda (httpcon)
    (require 'redis-config)
    (message "elnode-cache-server, cache-save")
    (elnode-http-start httpcon 200 '("Content-Type" . "text/html"))
    (elnode-http-return httpcon (r-save))))

(reset-my-default-elnode-url-mapping-table)

(my-elnode-add-handlers
 `(("^/orgs/\\(.*\\)$" . ,(org-dir-compiled-handler-maker "~/org/doc/"))
   ("^/homo/\\(.*\\.html\\)$" . ,(elnode-webserver-handler-maker "~/org/homo_public_html/"))
   ("^/homo/\\(.*\\.*\\)$" . ,(org-dir-compiled-handler-maker "~/org/homogenius/"))
   ("^/org/switch/\\(.*\\)$" . ,(byte-compile (elnode-switch-org)))
   ("^/url/encode/\\(.*\\)$" . ,(byte-compile (elnode-url-encoder)))
   ("^/url/decode/\\(.*\\)$" . ,(byte-compile (elnode-url-decoder)))
   ("^/cache/set/\\(.*\\)$" . ,(byte-compile (elnode-cache-setter)))
   ("^/cache/keys/\\(.*\\)$" . ,(byte-compile (elnode-cache-keys)))
   ("^/cache/del/\\(.*\\)$" . ,(byte-compile (elnode-cache-delete)))
   ("^/cache/get/\\(.*\\)$" . ,(byte-compile (elnode-cache-getter)))
   ("^/cache/save/\\(.*\\)$" . ,(byte-compile (elnode-cache-saver)))))

(defun open-org-file-by-elnode ()
  "."
  (interactive)
  (require 'ivy)
  (let* ((directory (expand-file-name "~/org/doc"))
         (org-files (directory-files directory t ".*?\\.org$")))
    (ivy-read "The org files: " (reverse org-files) :action
              #'(lambda (org-file)
                  (let* ((relative-path (file-relative-name org-file directory))
                         (url (format "http://%s:%s/orgs/%s" *my-default-elnode-host* *my-default-elnode-port* relative-path)))
                    (browse-url url))))))

(defun open-current-org-file-by-elnode ()
  "."
  (interactive)
  (let* ((directory (expand-file-name "~/org/doc"))
         (org-file (buffer-file-name (current-buffer))))
    (let* ((relative-path (file-relative-name org-file directory))
           (url (format "http://%s:%s/orgs/%s" *my-default-elnode-host* *my-default-elnode-port* relative-path)))
      (browse-url url))))

(provide 'el-server-extend)
;;; el-server-extend.el ends here