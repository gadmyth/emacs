;;; org-mode+.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2022 gadmyth

;; Author: org-mode+.el <gadmyth@gmail.com>
;; Version: 1.0.3
;; Package-Version: 20220808.001
;; Package-Requires: org, browse-url+
;; Keywords: org-mode
;; Homepage: https://www.github.com/gadmyth/emacs
;; URL: https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/org-mode+.el

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Source code
;;
;; org-mode+'s code can be found here:
;;   https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/org-mode+.el

;;; Commentary:
;;; Code:

(require 'dired)
(require 'browse-url+)
(require 'ol)

(defvar *org-cap-temp*)
(defvar *temp-org-capture-buffer* nil)
(defvar show-temp-capture-buffer-p nil)
(defvar *org-element-link-temp*)

(defun display-temp-capture-buffer ()
  "."
  (if (not (buffer-live-p *temp-org-capture-buffer*))
      (setq *temp-org-capture-buffer*
            (generate-new-buffer "*temp-org-capture*")))
  (display-buffer *temp-org-capture-buffer*)
  (select-window (get-buffer-window *temp-org-capture-buffer*))
  (org-mode))

(defun org-capture-with-random-mark (description)
  "DESCRIPTION: ."
  (interactive "sSet the line description here: ")
  (save-excursion
    (beginning-of-line)
    (newline)
    (forward-line -1)
    (let* ((line (replace-regexp-in-string "-" "" (org-id-uuid)))
           (filename (replace-regexp-in-string (getenv "HOME") "${HOME}" (buffer-file-name)))
           (link (format "file:%s::%s" filename line)))
      (insert (format "MARK: %s" line))
      (indent-for-tab-command)
      (comment-line 1)
      (setq *org-cap-temp* `((link . ,link) (description . ,description)))
      (when show-temp-capture-buffer-p
        (display-temp-capture-buffer)))))

(defun org-capture-current-file (description)
  "DESCRIPTION: ."
  (interactive "sSet the line description here: ")
  (let* ((filename (replace-regexp-in-string (getenv "HOME") "${HOME}" (buffer-file-name)))
         (link (format "file:%s" filename)))
    (setq *org-cap-temp* `((link . ,link) (description . ,description)))
    (when show-temp-capture-buffer-p
      (display-temp-capture-buffer))))

(defun org-capture-current-line (description)
  "DESCRIPTION: ."
  (interactive "sSet the line description here: ")
  (re-search-backward "^" nil t)
  ;; ignore the leading whitespace
  ;; ignore the left paren after the leading whitespace, for org-mode will parse it as coderef
  (re-search-forward "^[ \t(]*\\(.*?\\)$" nil t 1)
  (let* ((line (match-string-no-properties 1))
         (encoded-line (url-encode-url line))
         (filename (replace-regexp-in-string (getenv "HOME") "${HOME}" (buffer-file-name)))
         (link (format "file:%s::%s" filename line)))
    (setq *org-cap-temp* `((link . ,link) (description . ,description)))
    (when show-temp-capture-buffer-p
      (display-temp-capture-buffer))))

(defvar *org-capture-actions*
  '(("capture current line" . org-capture-current-line)
    ("capture new mark" . org-capture-with-random-mark)
    ("capture current file" . org-capture-current-file)))

(defun org-capture-mark ()
  "."
  (interactive)
  (let* ((action (completing-read "Please choose the action: "
                                  *org-capture-actions* nil t nil nil nil))
         (func (alist-get action *org-capture-actions* nil nil #'string-equal)))
    (message "%S" func)
    (when func
      (call-interactively func))))

(defun org-capture-insert-temp ()
  "."
  (interactive)
  (when (> (length *org-cap-temp*) 0)
    (let* ((link (alist-get "link" *org-cap-temp* nil nil #'string-equal))
           (description (alist-get "description" *org-cap-temp* nil nil #'string-equal))
           (content (if (> (length description) 0)
                        (format "[[%s][%s]]" link description)
                      link)))
      (insert content)
      (setq *org-cap-temp* nil))))

(defun org-capture-dired-file (description)
  "DESCRIPTION: ."
  (interactive "sSet the dir description here: ")
  (let* ((file (dired-get-file-for-visit))
         (link (format "file:%s" file)))
    (setq *org-cap-temp* `((link . ,link) (description . ,description)))
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
                               (file-name-directory org-link-file)))
               (file (if (file-name-absolute-p org-link-file) org-link-file
                       (expand-file-name org-link-file default-directory)))
               (dir org-link-dir))
          (cons file dir))
      (cons nil nil))))

(defun org-make-element-link ()
  "."
  (interactive)
  (let* ((filename (buffer-file-name))
         (properties (org-entry-properties))
         (item (alist-get "ITEM" properties nil nil #'string-equal))
         (custom-id (alist-get "CUSTOM_ID" properties nil nil #'string-equal)))
    (when (null custom-id)
      (setq custom-id (upcase (org-id-uuid)))
      (org-set-property "CUSTOM_ID" custom-id))
    (when (and
           (not (null item))
           (not (null custom-id)))
      (let ((link (format "[[file:%s::#%s][%s]]" filename custom-id item)))
        (message "org element link copied: %s" link)
        (setq *org-element-link-temp* link)))))

(defun org-insert-element-link ()
  "."
  (interactive)
  (when *org-element-link-temp*
    (let* ((dir (file-name-directory (buffer-file-name)))
           (link (replace-regexp-in-string dir "" *org-element-link-temp*)))
      (insert link)
      (setq *org-element-link-temp* nil))))

(defun org-open-dir ()
  "."
  (interactive)
  (let* ((parse-result (org-parse-file-link))
         (file (car parse-result))
         (dir (cdr parse-result)))
    (message "file: %s, dir: %s" file dir)
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

(defun org-parse-link ()
  "."
  (save-excursion
    (re-search-backward "^" nil t)
    (if (re-search-forward "^.*?\\(\\(https?\\|file\\):[^]]*\\).*$" nil t 1)
        (let* ((org-link (match-string-no-properties 1)))
          org-link))))

(defun org-copy-link ()
  "."
  (interactive)
  (when-let ((org-link (org-parse-link)))
    (kill-new org-link)
    (message org-link)))

(defvar *org-link-program-alist*
  `(("\\.\\(jpe?g\\|png\\)\\'"
     ,(cond ((eq window-system 'x)
             '("viewnior" "ristretto"))
            (t nil)))
    ("\\.xlsx\\'"
     ,(cond ((eq window-system 'x)
             '("gnumeric"))
            (t nil)))
    ("\\.htm?l\\'"
     ,(cond ((eq window-system 'x)
             '("google-chrome" "firefox"))
            (t nil)))
    ("\\https?://.*\\'"
     ,(cond ((eq window-system 'x)
             '("google-chrome" "firefox"))
            (t nil)))))

(defun org-open-link-external ()
  "."
  (interactive)
  (when-let ((org-link (org-parse-link)))
    (cond
     ((string-prefix-p "http" org-link)
      (browse-url-select-function org-link))
     ((string-prefix-p "file:" org-link)
      (let* ((path (substring org-link (length "file:")))
             (path (or (and (file-name-absolute-p path) path)
                       (format "%s/%s" default-directory path)))
             (programs (org-filter-link-programs path))
             (program (completing-read "Select the command: " programs nil t)))
        (message "file path is %s" path)
        (when program
          (start-process program nil program path)))))))

(defun org-filter-link-programs (param)
  "Filter the programs of PARAM with match the alist's REGEXP."
  (-flatten
   (mapcar #'cdr
           (seq-filter
            (lambda (elem)
              (string-match-p (car elem) param))
            *org-link-program-alist*))))

(defun enhance-org-open-link ()
  "."
  (cond
   ((bound-and-true-p *enhance-org-open-link*)
    ;; copied and modified from ol.el.gz
    (dolist (scheme '("http" "https"))
      (org-link-set-parameters scheme
                               :follow
                               `(lambda (url)
                                  (browse-url-select-function (concat ,scheme ":" url))))))
   (t
    ;; copied from ol.el.gz
    (dolist (scheme '("http" "https"))
      (org-link-set-parameters scheme
			                   :follow
			                   `(lambda (url arg)
			                      (browse-url (concat ,scheme ":" url) arg)))))))

(enhance-org-open-link)

(defun org-copy-block-contents ()
  "."
  (interactive)
  (when-let ((content (plist-get (cadr (org-element-at-point)) :value)))
    (kill-new content)
    (message "Org block content copied!")))

(global-set-key (kbd "<f7>") 'org-capture-mark)
(global-set-key (kbd "<M-f7>") 'org-capture-insert-temp)
(global-set-key (kbd "<f8>") 'org-make-element-link)
(global-set-key (kbd "<M-f8>") 'org-insert-element-link)
(global-set-key (kbd "C-c d") 'org-open-dir)
(global-set-key (kbd "C-c P") 'org-show-link)
;; config for create a link with id property
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-.") 'org-time-stamp-inactive)
(global-set-key (kbd "C-c w") 'org-copy-block-contents)

(provide 'org-mode+)
;;; org-mode+.el ends here
