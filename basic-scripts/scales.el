;;; package --- scales.el
;;; Commentary:
;;; Code:


;; scale-amount has been defined in workspace.el

(defvar *mac-scale-amount* 0)
(defvar *linux-scale-amount* 0)

(defun scale-large (&optional amount)
  "AMOUNT: ."
  (let* ((system-amount
          (cond ((eq window-system 'ns)
                 *mac-scale-amount*)
                (t
                 *linux-scale-amount*)))
          (scale-amount
           (or amount system-amount)))
    (text-scale-adjust scale-amount)))

(provide 'scales)
;;; scales.el ends here
