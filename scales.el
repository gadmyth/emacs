;; scale-amount has been defined in workspace.el

(defvar *mac-scale-amount* 2)
(defvar *linux-scale-amount* 3)
(defun scale-large (&optional amount)
  (let ((scale-amount
		 (if amount amount
		   (if (eq window-system 'ns) *mac-scale-amount* *linux-scale-amount*))))
    (text-scale-adjust scale-amount)))


(provide 'scales)