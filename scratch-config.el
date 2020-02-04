;;; package --- scratch-config.el
;;; Commentary:
;;; Code:


(defvar +scratch-file-name+ (expand-file-name "~/.emacs.scratch"))
(add-to-list 'auto-coding-alist '("\\.scratch\\'" . utf-8))

(add-hook 'emacs-startup-hook #'(lambda () (load-scratch-from-file t)))
(add-hook 'kill-emacs-hook 'save-scratch-buffer)

(defun load-scratch-from-file (&optional override)
  "Load scratch file to *scratch* buffer, if OVERRIDE is t, erase buffer first."
  (interactive)
  (if (not (file-exists-p +scratch-file-name+))
      (message "Can't load %s file, for it does not exist!" +scratch-file-name+)
    (let ((buffer (get-buffer-create "*scratch*")))
      (with-current-buffer
          buffer
        (if override (erase-buffer))
        (insert-file-contents +scratch-file-name+)))))



(defun save-scratch-buffer ()
  "Save *scatch* buffer to file."
  (interactive)
  (let ((buffer (get-buffer "*scratch*")))
    (if (not buffer)
        (message "buffer <*scratch*> does not exist!")
      (with-current-buffer buffer
        (let ((content (buffer-substring-no-properties (point-min) (point-max))))
          (with-temp-file +scratch-file-name+
            (insert content)))))))

(provide 'scratch-config)
;;; scratch-config.el ends here
