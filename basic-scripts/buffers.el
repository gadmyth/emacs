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

(defun switch-scratch-buffers (&rest _)
  "NAME: ."
  (interactive)
  (let* ((scratch-buffers (seq-filter (lambda (buffer) (string-prefix-p "*scratch*" (buffer-name buffer))) (buffer-list)))
         (name (completing-read "Switch to buffer: " (sort (mapcar #'buffer-name scratch-buffers) 'string<) nil t nil nil nil))
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
