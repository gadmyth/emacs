;;; package --- files-config.el
;;; Commentary:
;;; Code:

(defun revert-buffer-with-default-coding-system ()
  "."
  (let ((coding-system-for-read 'utf-8-unix))
    (revert-buffer t t t)))

(add-hook 'find-file-hook
          (lambda ()
            (revert-buffer-with-default-coding-system)
            (scale-large)
            (require-if-installed 'textmate (textmate-mode))
            (require-if-installed 'xcscope (cscope-minor-mode))
            (display-line-numbers-mode (if (equal major-mode 'org-mode) 0 1))
            ))

(add-hook 'after-save-hook
          (lambda () (if (string= (buffer-name) ".emacs")
                         (byte-compile-file (expand-file-name "~/.emacs")))))

(add-hook 'after-init-hook 'global-flycheck-mode)

(setq backup-directory-alist (quote (("." . "~/.backups"))))

(provide 'files-config)
;;; files-config.el ends here
