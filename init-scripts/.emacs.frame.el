;;; package --- .emacs.frame.el
;;; Commentary:
;;; Code:

;; load path
(emacs-load-path-initialize)

(require 'package-tools)
(require 'q)

;; load script files at first
(require-package
 'script-extends
 (load-pre-script-files))

;; insall packages
(require-package
 'packages
 (package-initialize)
 (install-packages-if-needed +required-packages+))

;; load configs
(require 'loading-config)
(require 'basic-config)
(require 'proced-config)
(require 'servers)
(require 'frames)
(require 'windows)
(require 'buffers)
(require 'mode-bars)
(require 'fonts)
(require 'shells)
(require 'irc-config)
(require 'telega-config)
(require 'tab-config)
(require 'clipboard)
;(require 'codings)
(require 'codec)
(require 'abbrev-config)
(require 'uniquify-config)
(require 'version-controll)
(require 'swiper-config)
(require 'org-config)
(require 'gkroam-config)
(require 'ido-config)
(require 'eww-config)
(require 'slime-config)
(require 'projectile-config)
(require 'scales)
(require 'files-config)
;(require 'evil-config)
(require 'f-zone-key-bindings)
(require 'calendar-config)
(require 'dired-config)
(require 'emoji-config)
;; download package
(require 'web-config)
(require 'smart-compile-config)
(require 'python-config)
(require 'lsp-config)
;; package + post config, in a seperate repo?? not in emacs.git, but should keep in cvs
(require 'youdao-dictionary-conf)
(require 'nov-config)
;; self config, require safely
(require 'pdf-config)
(require 'vc-config)
(require-package 'redis-config :dependencies '(eredis))
(require-package 'annotate-config :dependencies '(annotate))
(require-package 'yas-config (yas-global-mode))
(require 'anythings)
(require 'exwm-conf)

;; -------
;; network-util, rest-util, minibuffer+
;; -------
(require 'q)
(require 'utility)
(require 'dates)
(require 'elisp+)
(require 'el-server)
(require 'el-server-extend)
(require 'org-mode+)
(require 'goto-last-point+)
(require 'scratch+)
(require 'list-scratch)
(require-package 'notifications :dependencies '(uuid))
(require-package 'customized-dir (customized-dir-init))
(require-package 'stopwatch :dependencies '(switch-buffer-functions) (stopwatch-mode 1))
(require-package 'eyebrowse+ (eyebrowse-plus-mode 1))
(require-package 'eyebrowse-xmonad (eyebrowse-xmonad-mode 1))
(require-package 'wcy-desktop (wcy-desktop-init))
(require-package 'password-generator+ :dependencies '(password-generator))


;; the following should move to el-post-scripts
;; package + post config
(require 'sudo-edit)
(require-package 'smartparens (smartparens-global-mode))
(require-package 'expand-region (global-set-key (kbd "C-=") 'er/expand-region))
(require-package 'alpha (transparency-set-value *default-trans-value*))
(require-package 'smex (global-set-key (kbd "M-x") 'smex))
(require-package 'auto-complete (global-auto-complete-mode))

(eval-after-load "textmate" '(add-to-list '*textmate-project-roots* ".svn"))
(eval-after-load "xcscope" '(ignore-errors (add-to-list 'cscope-indexer-suffixes "*.java")))
(require-package 'network-util (turn-off-env-proxy))

;; load must files
(load-must-files)

;; load script files
(load-extend-script-files)

;; load script files at last
(load-post-script-files)

(provide '.emacs.frame)
;;; .emacs.frame.el ends here
