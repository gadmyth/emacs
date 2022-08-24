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

(defvar *quick-buffer-prefix-list* '("*scratch*" "*Messages*" "*Help*" "*terminal" "*eshell*" "*shell*"))

(defun switch-scratch-buffers (&rest _)
  "NAME: ."
  (interactive)
  (let* ((quick-buffers (seq-filter (lambda (buffer)
                                      (let ((matched-list (mapcar
                                                           (lambda (name)
                                                             (string-prefix-p name (buffer-name buffer)))
                                                           *quick-buffer-prefix-list*)))
                                        (eval `(or ,@matched-list))))
                                    (buffer-list)))
         (quick-buffers (sort (mapcar #'buffer-name quick-buffers) 'string<))
         (quick-buffers (cons "*scratch*" (delete "*scratch*" quick-buffers)))
         (quick-buffer-names (mapcar (lambda (bname)
                                       (let* ((buffer (get-buffer bname))
                                              (mode (with-current-buffer buffer major-mode))
                                              (rich-name bname))
                                         (when (or (eq 'term-mode mode)
                                                   (eq 'shell-mode mode)
                                                   (eq 'eshell-mode mode))
                                           (setq rich-name (format "%s (%s)" bname (with-current-buffer buffer default-directory))))
                                         (cons rich-name bname)))
                                     quick-buffers))
         (name (completing-read "Switch to buffer: " quick-buffer-names nil t nil nil nil))
         (buffer (get-buffer (assoc-default name quick-buffer-names #'string=))))
    (if (buffer-live-p buffer)
        (progn
          (switch-to-buffer buffer)
          (setq buffer-offer-save t))
      (message (format "The buffer %s doesn't exist!" name)))))

(global-set-key (kbd "s-<left>") #'previous-buffer)
(global-set-key (kbd "s-<right>") #'next-buffer)

(provide 'buffers)
;;; buffers.el ends here
