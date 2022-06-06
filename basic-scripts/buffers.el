;;; package --- buffers.el
;;; Commentary:
;;; Code:


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

(defvar *quick-buffer-prefix-list* '("*scratch*" "*Messages*" "*Help*" "*terminal"))

(defun switch-scratch-buffers (&rest _)
  "NAME: ."
  (interactive)
  (let* ((quick-buffers (seq-filter (lambda (buffer)
                                      (let ((matched-list (mapcar (lambda (name) (string-prefix-p name (buffer-name buffer))) *quick-buffer-prefix-list*)))
                                        (eval `(or ,@matched-list))))
                                    (buffer-list)))
         (quick-buffers (sort (mapcar #'buffer-name quick-buffers) 'string<))
         (quick-buffers (cons "*scratch*" (delete "*scratch*" quick-buffers)))
         (name (completing-read "Switch to buffer: " quick-buffers nil t nil nil nil))
         (buffer (get-buffer name)))
    (if (buffer-live-p buffer)
        (progn
          (switch-to-buffer buffer)
          (setq buffer-offer-save t))
      (message (format "The buffer %s doesn't exist!" name)))))

(global-set-key (kbd "s-<left>") #'previous-buffer)
(global-set-key (kbd "s-<right>") #'next-buffer)

(provide 'buffers)
;;; buffers.el ends here
