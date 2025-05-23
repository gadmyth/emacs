;;; package --- ido-config.el
;;; Commentary:
;;; Code:

(ido-mode t)
;; origin is raise-frame: if buffer is in another frame, just raise the frame
(setq ido-default-buffer-method nil)
;; (ido-everywhere t)
;; (setq ido-enable-flex-matching t)

;; (require 'ido-completing-read+)
;; (ido-ubiquitous-mode 1)

(provide 'ido-config)
;;; ido-config.el ends here
