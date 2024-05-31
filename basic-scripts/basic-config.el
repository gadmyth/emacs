;;; package --- basic-config.el
;;; Commentary:
;;; Code:

(defalias 'yes-or-no-p 'y-or-n-p)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(global-unset-key (kbd "C-SPC"))
(setq x-alt-keysym 'meta)
(setq inhibit-startup-message t)
(setq max-specpdl-size 1024)
(setq max-lisp-eval-depth 1024)

;; when kill emacs, should be confirmed by hand
(setq confirm-kill-emacs 'yes-or-no-p)

(require 'warnings)
(setq display-warning-minimum-level :emergency)

(if (boundp 'tool-bar-mode)
    (tool-bar-mode (bound-or-default *tool-bar-mode* t)))

(if (boundp 'menu-bar-mode)
    (menu-bar-mode (bound-or-default *menu-bar-mode* t)))

(if (boundp 'blink-cursor-mode)
    (blink-cursor-mode (bound-or-default *blink-cursor-mode* t)))

(if (boundp 'scroll-bar-mode)
    (scroll-bar-mode (bound-or-default *scroll-bar-mode* t)))

(put 'scroll-left 'disabled nil)

(provide 'basic-config)
;;; basic-config.el ends here