;;; package --- eyebrowse-config.el
;;; Commentary:
;;; Code:

(require 'eyebrowse+)

(eyebrowse-mode t)
(eyebrowse-rename-window-config 1 "default")

(add-hook 'window-setup-hook
          #'eyebrowse-lazy-load-config)

(add-hook 'eyebrowse-lazy-load-hook
          (lambda ()
            (add-hook 'kill-emacs-hook #'save-eyebrowse-config)))

(provide 'eyebrowse-config)
;;; eyebrowse-config.el ends here
