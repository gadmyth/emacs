;;; package --- follows.el
;;; Commentary:
;;; Code:

(defun make-follow(number)
  (interactive "nNumber of split window: ")
  (delete-other-windows)
  (dotimes (a (- number 1) [])
    (split-window-right))
  (balance-windows)
  (follow-mode))

(defun make-soft-follow ()
  "."
  (interactive)
  (save-excursion
    (let ((buffer (current-buffer)))
      (balance-windows)
      (follow-mode t)
      (dolist (window (window-list))
        (window--display-buffer buffer window 'reuse nil)))))

(provide 'follows)
;;; follows.el ends here