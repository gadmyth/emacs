;;; package --- org-mode+.el
;;; Commentary:
;;; Code:


(defvar *org-cap-temp*)
(defvar *temp-org-capture-buffer* nil)

(defun display-temp-capture-buffer ()
  "."
  (if (not (buffer-live-p *temp-org-capture-buffer*))
      (setq *temp-org-capture-buffer*
            (generate-new-buffer "*temp-org-capture*")))
  (display-buffer *temp-org-capture-buffer*)
  (select-window (get-buffer-window *temp-org-capture-buffer*))
  (org-mode))

(defun org-capture-current-line (description)
  "DESCRIPTION: ."
  (interactive "sSet the line description here: ")
  (re-search-backward "^" nil t)
  ;; ignore the leading whitespace
  ;; ignore the left paren after the leading whitespace, for org-mode will parse it as coderef
  (re-search-forward "^[ \t(]*\\(.*?\\)$" nil t 1)
  (let* ((line (match-string-no-properties 1))
         (encoded-line (url-encode-url line))
         (formatted-line (format "[[file:%s::%s][%s]]" (buffer-file-name) encoded-line description)))
    (setq *org-cap-temp* formatted-line)
    (display-temp-capture-buffer)))

(defun org-capture-insert-temp ()
  "."
  (interactive)
  (if (not (null *org-cap-temp*))
      (progn
        (insert *org-cap-temp*)
        (setq *org-cap-temp* nil))))

(defun org-capture-dired-file (description)
  "DESCRIPTION: ."
  (interactive "sSet the dir description here: ")
  (let* ((file (dired-get-file-for-visit))
        (formatted-line (format "[[file:%s][%s]]" file description)))
    (setq *org-cap-temp* formatted-line)
    ;(visit-work-file)
    ))

(defun org-parse-file-link ()
  "."
  (interactive)
  (save-excursion
    (re-search-backward "^" nil t)
    (if (re-search-forward "^.*?file:\\([^]:]*\\).*$" nil t 1)
        (let* ((org-link-file (match-string-no-properties 1))
               (org-link-dir (if (file-directory-p org-link-file)
                                 org-link-file
                               (file-name-directory org-link-file))))
          (cons org-link-file org-link-dir))
      (cons nil nil))))

(defun org-open-dir ()
  "."
  (interactive)
  (let* ((parse-result (org-parse-file-link))
         (file (car parse-result))
         (dir (cdr parse-result)))
    (when (and (not (null file))
               (not (null dir)))
      (dired-other-window dir)
      (dired-goto-file file)
      (message "Selected file is: %s" file))))

(defun org-show-link ()
  "."
  (interactive)
  (let* ((parse-result (org-parse-file-link))
         (file (car parse-result))
         (dir (cdr parse-result)))
    (if (and (not (null file)) (not (null dir)))
      (message "The file of link is: %s" file))))

(defun org-copy-link ()
  "."
  (interactive)
  (save-excursion
    (re-search-backward "^" nil t)
    (if (re-search-forward "^.*?\\(https?:[^]]*\\).*$" nil t 1)
        (let* ((org-link (match-string-no-properties 1)))
          (kill-new org-link)
          (message org-link)
          org-link))))

(global-set-key (kbd "<f7>") 'org-capture-current-line)
(global-set-key (kbd "<f8>") 'org-capture-insert-temp)
(global-set-key (kbd "C-c d") 'org-open-dir)
(global-set-key (kbd "C-c P") 'org-show-link)

(provide 'org-mode+)
;;; org-mode+.el ends here