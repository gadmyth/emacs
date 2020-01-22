;;; package --- clipboard.el
;;; Commentary:
;;; Code:

(when (eq window-system 'x)
  (setq x-select-enable-clipboard t)
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
  (setq x-stretch-cursor t)
  )

(provide 'clipboard)
;;; clipboard.el ends here