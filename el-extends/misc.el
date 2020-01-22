;;; package --- misc.el
;;; Commentary:
;;; Code:

(defun int-to-binary-string (input-number)
  "INPUT-NUMBER: ."
  "Convert an integer I into it's binary representation in string format."
  (interactive "nInut an integer: ")
  (let ((i input-number)
        (binary ""))
    (while (not (= i 0))
      (setq binary (concat (if (= 1 (logand i 1)) "1" "0") binary))
      (setq i (lsh i -1)))
    (if (string= binary "")
        (setq binary "0"))
    (message "convert %d to binary %S" input-number binary)
    binary))

(provide 'misc)
;;; misc.el ends here