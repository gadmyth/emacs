;;; package --- abbrev-config.el
;;; Commentary:
;;; Code:

(require 'abbrev)

(setq default-abbrev-mode t)
(setq save-abbrevs t)

(defun add-abbrev-hook (major-mode-name abbrev-alist)
  "When a MAJOR-MODE-NAME is loaded, define the abbrevs with ABBREV-ALIST in the major mode hook."
  (let* ((hook-name (format "%s-hook" major-mode-name))
         (hook-sym (intern hook-name)))
    (add-hook hook-sym
              (lambda()
                (progn (mapc (lambda (pair)
                               (define-abbrev objc-mode-abbrev-table (car pair) (cdr pair)))
                             abbrev-alist))))))

(defvar *abbrev-default-directory* (expand-file-name "abbrevs" +emacs-context-directory+))

(add-to-list 'auto-coding-alist '("\\.abbrev\\'" . utf-8))
;; TODO: add major-mode config for xxx-mode.abbrev file

(defun load-abbrevs ()
  "."
  (mapc (lambda (file-full-path)
          (let* ((relative-path (file-name-nondirectory file-full-path))
                 (mode-name (file-name-sans-extension relative-path)))
            (with-temp-buffer
              (insert-file-contents +eyebrowse-file-name+)
              (goto-char (point-min))
              (let* ((content (read (current-buffer)))
                     (alist content))
                (add-abbrev-hook mode-name alist)))))
        (directory-files *abbrev-default-directory* t ".*?\\.abbrev$")))

(load-abbrevs)

(setq save-abbrevs nil)

(provide 'abbrev-config)
;;; abbrev-config.el ends here
