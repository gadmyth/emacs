;;; package --- abbrev-config.el
;;; Commentary:
;;; Code:

(require 'abbrev)

(setq save-abbrevs t)

(defvar abbrev-tab-functions nil)

(defun tab-expand-abbrev-first ()
  "."
  (interactive)
  (unless (expand-abbrev)
    (let ((func (alist-get major-mode abbrev-tab-functions)))
      (message "expand-abbrev failed, now call function: %S" func)
      (when func
        (funcall func)))))

(defun bind-expand-abbrev-with-tab ()
  "."
  (interactive)
  (let* ((key (kbd "<tab>"))
         (tab-function (key-binding key t)))
    (when (not (function-equal tab-function #'tab-expand-abbrev-first))
      (add-to-list 'abbrev-tab-functions (cons major-mode tab-function))
      (local-set-key key #'tab-expand-abbrev-first))))

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

(defvar *abbrev-config-special-major-modes* (list 'emacs-lisp-mode 'elisp-mode
                                                  'org-mode 'org
                                                  nil))

(defun load-abbrev-file ()
  "Load abbrev file of MAJOR-MODE."
  (interactive)
  (let* ((file-full-path (format "%s/%s.abbrev" *abbrev-default-directory* major-mode))
         (major-mode-name (symbol-name major-mode))
         (major-mode-package (plist-get *abbrev-config-special-major-modes* major-mode)))
    (if (not major-mode-package) (setq major-mode-package major-mode))
    (message "*** load-abbrev-file, major-mode-name: %S, package: %S, file path: %S" major-mode-name major-mode-package file-full-path)
    (when (and major-mode-package
               (file-exists-p file-full-path))
      (with-temp-buffer
        (insert-file-contents file-full-path)
        (goto-char (point-min))
        (let* ((content (read (current-buffer)))
               (alist content))
          (when (or
                 (featurep major-mode-package)
                 (package-installed-p major-mode-package)
            (require major-mode-package)
            (add-abbrev-hook major-mode-name alist)))))))

(setq save-abbrevs nil)

(provide 'abbrev-config)
;;; abbrev-config.el ends here
