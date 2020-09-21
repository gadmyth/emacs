;;; package --- buffers.el
;;; Commentary:
;;; Code:

(defvar *mac-scale-amount* 0)
(defvar *linux-scale-amount* 0)

(global-linum-mode 0)
(global-visual-line-mode t)
(global-auto-revert-mode t)

(show-paren-mode 1)
(setq column-number-mode t)
(setq line-number-mode t)
(setq size-indication-mode t)

(defun new-buffer (name)
  "NAME: ."
  (interactive (list (read-string "Create buffer(*scratch*): " nil nil "*scratch*")))
  (let ((buffer (generate-new-buffer name)))
    (switch-to-buffer buffer)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))

(defun switch-buffer-default-scratch (&rest _)
  "NAME: ."
  (interactive)
  (let* ((name (read-string "Switch buffer(*scratch*): " nil nil "*scratch*"))
         (buffer (get-buffer name)))
    (if (buffer-live-p buffer)
        (progn
          (switch-to-buffer buffer)
          ;(funcall (and initial-major-mode))
          (setq buffer-offer-save t))
      (message (format "The buffer %s doesn't exist!" name)))))

(global-set-key (kbd "s-<left>") #'previous-buffer)
(global-set-key (kbd "s-<right>") #'next-buffer)

(provide 'buffers)
;;; buffers.el ends here
