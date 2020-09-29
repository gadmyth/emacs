;;; package --- abbrev-config.el
;;; Commentary:
;;; Code:

(require 'abbrev)

(setq save-abbrevs t)

(defun add-abbrev-hook (major-mode-name abbrev-alist)
  "When a MAJOR-MODE-NAME is loaded, define the abbrevs with ABBREV-ALIST in the major mode hook."
  (mapc (lambda (pair)
  (let* ((table-name (format "%s-abbrev-table" major-mode-name))
         (table-sym (intern table-name))
         (table (symbol-value table-sym)))
    (define-abbrev table (car pair) (cdr pair))))
        abbrev-alist))

(defvar *abbrev-default-directory* (expand-file-name "abbrevs" +emacs-context-directory+))

(add-to-list 'auto-coding-alist '("\\.abbrev\\'" . utf-8))
;; TODO: add major-mode config for xxx-mode.abbrev file

(defun load-abbrevs ()
  "."
  (mapc (lambda (file-full-path)
          (let* ((relative-path (file-name-nondirectory file-full-path))
                 (major-mode-name (file-name-sans-extension relative-path)))
            (with-temp-buffer
              (insert-file-contents file-full-path)
              (goto-char (point-min))
              (let* ((content (read (current-buffer)))
                     (alist content))
                (add-abbrev-hook major-mode-name alist)))))
        (directory-files *abbrev-default-directory* t ".*?\\.abbrev$")))

(load-abbrevs)

(setq save-abbrevs nil)

(provide 'abbrev-config)
;;; abbrev-config.el ends here
