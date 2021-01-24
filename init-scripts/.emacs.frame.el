;;; package --- .emacs.frame.el
;;; Commentary:
;;; Code:

;; load path
(add-to-list 'load-path (expand-file-name "basic-scripts" +emacs-context-directory+))
(add-to-list 'load-path (expand-file-name "el-pre-scripts" +emacs-context-directory+))
(add-to-list 'load-path (expand-file-name "el-extends" +emacs-context-directory+))
(add-to-list 'load-path (expand-file-name "el-post-scripts" +emacs-context-directory+))

;; load script files at first
(require 'script-extends)
(load-pre-script-files)

;; insall packages
(require 'package-tools)
(require 'packages)
(install-packages-if-needed +required-packages+)

;; load configs
(require 'loading-config)
(require 'basic-config)
(require 'utility)
(require 'frames)
(require 'windows)
(require 'buffers)
(require 'mode-bars)
(require 'fonts)
(require 'themes)
(require 'shells)
(require 'irc-config)
(require 'tab-config)
(require 'clipboard)
;(require 'codings)
(require 'codec)
(require 'dates)
(require 'abbrev-config)
(require 'anythings)
(require 'uniquify-config)
(require 'version-controll)
(require 'swiper-config)
(require 'elisp+)
(require 'org-mode+)
(require 'org-config)
(require 'gkroam-config)
(require 'ido-config)
(require 'eww-config)
(require 'slime-config)
(require 'projectile-config)
(require 'scales)
(require 'files-config)
;(require 'evil-config)
(require 'el-server)
(require 'el-server-extend)
(require 'servers)
(require 'goto-last-point+)
(require 'f-zone-key-bindings)
(require 'holiday-config)
(require 'dired-config)
(require 'emoji-config)
(require 'sudo-edit)
(require 'web-config)
(require 'smart-compile-config)
(require 'python-config)
(require 'scratch+)
(require 'notifications)
(require 'lsp-config)

(require-if-installed 'eredis (require 'redis-config))
(require-if-installed 'smartparens (smartparens-global-mode))
(require-if-installed 'expand-region (global-set-key (kbd "C-=") 'er/expand-region))
(require-if-installed 'alpha (transparency-set-value *default-trans-value*))
(require-if-installed 'smex (global-set-key (kbd "M-x") 'smex))
(require-package 'yas-config (yas-global-mode))
(require-package 'auto-complete (global-auto-complete-mode))
(require-package 'annotate (require 'annotate-config))
(require-package 'customized-dir (customized-dir-init))
(require-package 'eyebrowse+ (eyebrowse-plus-mode t))
(require-package 'wcy-desktop (wcy-desktop-init))
(require 'exwm-conf)

(eval-after-load "textmate" '(add-to-list '*textmate-project-roots* ".svn"))
(eval-after-load "xcscope" '(ignore-errors (add-to-list 'cscope-indexer-suffixes "*.java")))
(switch-proxy nil)

;; load must files
(load-must-files)

;; load script files
(load-extend-script-files)

;; load script files at last
(load-post-script-files)

;; maximize the frame
(toggle-frame-maximized)

(provide '.emacs.frame)
;;; .emacs.frame.el ends here
