;;; package --- .emacs.simple.el
;;; Commentary:
;;; Code:

;; load path
(emacs-load-path-initialize)

(require 'package-tools)
(require 'q)

;; load script files at first
(require-package 'script-extends
                 (load-pre-script-files))

;; init archives
(packages-init-archives)

;; load configs
(require 'loading-config)
(require 'basic-config)
(require 'mode-bars)

;; f-zone
(require 'minibuffer+)

;; auto complete
(require 'swiper-config)
(require-package 'ido-config
                 :dependencies '(ido-completing-read+))
(require-package 'smex
                 (global-set-key (kbd "M-x") 'smex))

(require 'windows)
(require 'dired-config)

;; editors
(require 'lines)
(require 'kill-ring)
(require-package 'expand-region
                 (global-set-key (kbd "C-=") 'er/expand-region))

(require-package 'yas-config
                 :dependencies '(yasnippet dash ivy)
                 (yas-global-mode))

(require-package 'smartparens
                 :dependencies '(dash)
                 (smartparens-global-mode))

(require 'at-point)

(require 'tab-config)
(require 'clipboard-config)
(require 'clipboard+)

(require 'scratch+)

(require 'minibuffer+)
;; list-scratch
(require-package 'list-scratch
                 (global-set-key (kbd "<f12>") (toggle-minibuffer 'list-scratch)))

;; vc
(require 'version-controll)

;; buffer
(require-package 'buffers
                 (global-set-key (kbd "<f1>") (toggle-minibuffer 'switch-scratch-buffers))
                 (global-set-key (kbd "<M-f1>") 'new-buffer))

(global-set-key (kbd "<f3>") (toggle-minibuffer 'ido-switch-buffer))

;; customized dir
(require-package 'customized-dir
                 (customized-dir-init)
                 (global-set-key (kbd "<f4>") (toggle-minibuffer 'switch-to-customized-dir)))

;; library
(global-set-key (kbd "<f6>") (toggle-minibuffer 'find-library))
(global-set-key (kbd "<M-f6>") (toggle-minibuffer 'require-library))
(global-set-key (kbd "<C-f6>") (toggle-minibuffer 'load-library))

;; load script files at last
(load-post-script-files)

;; in files-config.el
(setq make-backup-files nil)

;; load wcy buffers
(require-package 'wcy-desktop
                 (wcy-desktop-init))

(provide '.emacs.simple)
;;; .emacs.simple.el ends here
