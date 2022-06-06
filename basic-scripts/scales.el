;;; package --- scales.el
;;; Commentary:
;;; Code:


;; scale-amount has been defined in workspace.el

(defun scale-large (&optional amount)
  "AMOUNT: ."
  (let* ((system-amount
          (cond ((eq window-system 'ns)
                 (bound-or-default *mac-scale-amount* 0))
                (t
                 (bound-or-default *linux-scale-amount* 0))))
          (scale-amount
           (or amount system-amount)))
    (text-scale-adjust scale-amount)))

(provide 'scales)
;;; scales.el ends here
