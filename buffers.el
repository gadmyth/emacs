;;; package --- buffers.el
;;; Commentary:
;;; Code:


(defun new-buffer (name)
  "NAME: ."
  (interactive (list (read-string "Create buffer(*scratch*): " nil nil "*scratch*")))
  (let ((buffer (generate-new-buffer name)))
    (switch-to-buffer buffer)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))

(defun switch-buffer-default-scratch (name)
  "NAME: ."
  (interactive (list (read-string "Switch buffer(*scratch*): " nil nil "*scratch*")))
  (let ((buffer (get-buffer name)))
    (if (buffer-live-p buffer)
        (progn
          (switch-to-buffer buffer)
          ;(funcall (and initial-major-mode))
          (setq buffer-offer-save t))
      (message (format "The buffer %s doesn't exist!" name)))))

(provide 'buffers)
;;; buffers.el ends here
