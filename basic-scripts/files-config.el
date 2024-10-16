;;; package --- files-config.el
;;; Commentary:
;;; Code:

(require 'scales)

(defun revert-buffer-with-default-coding-system ()
  "."
  (let ((coding-system-for-read 'utf-8-unix))
    (when (file-exists-p buffer-file-name)
      (revert-buffer t t t))))

(defun find-file-post-action ()
  "."
  (revert-buffer-with-default-coding-system)
  (scale-large)
  (require-package 'textmate (textmate-mode))
  (require-package 'xcscope (cscope-minor-mode))
  (require-package 'annotate (annotate-mode))
  (display-line-numbers-mode (if (equal major-mode 'org-mode) 0 1)))

(add-hook 'find-file-hook #'find-file-post-action)

(defun compile-emacs-init-file ()
  "."
  (if (string= (buffer-name) ".emacs")
      (byte-compile-file (expand-file-name "~/.emacs"))))

(add-hook 'after-save-hook #'compile-emacs-init-file)

(require-package
  'flycheck
  (add-hook 'after-init-hook #'global-flycheck-mode))

(setq backup-directory-alist (quote (("." . "~/.backups"))))

(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))

(provide 'files-config)
;;; files-config.el ends here
