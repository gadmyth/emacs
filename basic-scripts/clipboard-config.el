;;; package --- clipboard-config.el
;;; Commentary:
;;; Code:

(when (eq window-system 'x)
  (setq select-enable-clipboard t)
  (setq x-select-enable-clipboard-manager nil)
  (setq interprogram-paste-function 'x-selection-value)
  (setq x-stretch-cursor t)
  )

(provide 'clipboard-config)
;;; clipboard-config.el ends here
