;;; package --- projectile-config.el
;;; Commentary:
;;; Code:


(when (package-installed-p 'projectile)
  (require 'projectile)
  (projectile-global-mode)
  (setq projectile-indexing-method 'native)
  (setq projectile-enable-caching t)
  (setq projectile-file-exists-remote-cache-expire nil))

(provide 'projectile-config)
;;; projectile-config.el ends here
