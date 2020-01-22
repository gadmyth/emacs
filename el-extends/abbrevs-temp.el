;;; package --- abbrevs-temp.el
;;; Commentary:
;;; Code:

(defun def-objc-abbrev (key value)
  "KEY: abbrev's key, VALUE: abbrev's value."
  (interactive "sPlease input the abbrev's key:\nsPlease input the abbrev's value:")
  (define-abbrev objc-mode-abbrev-table key value))

(provide 'abbrevs-temp)
;;; abbrevs-temp.el ends here