;;; package --- nov-config.el
;;; Commentary:
;;; Code:

(require-package
 'nov
 (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(provide 'nov-config)
;;; nov-config.el ends here
