;;; package --- .emacs.frame.el
;;; Commentary:
;;; Code:

;; load path
(emacs-load-path-initialize)

(require 'package-tools)
(require 'package-config)
(require 'q)

;; load script files at first
(require-package
 'script-extends
 (load-pre-script-files))

;; insall packages
;; should download packages
(require-package
 'packages
 (package-initialize))

;; load configs
(require 'loading-config)
(require 'basic-config)
(require 'proced-config)
(require 'servers)
(require 'utility)
(require 'at-point)
(require 'dates)
(require 'elisp+)
(require-package 'expand-region (global-set-key (kbd "C-=") 'er/expand-region))
(require-package 'smartparens :dependencies '(dash) (smartparens-global-mode))
(require 'scratch+)
(require 'list-scratch)
(require 'windows)
(require 'buffers)
(require 'mode-bars)
(require 'fonts)
(require 'shells)
(require 'tab-config)
(require 'clipboard-config)
(require 'clipboard+)
(require 'lines)
(require 'kill-ring)
(require 'org-mode+)
;(require 'codings)
(require 'codec)
(require 'abbrev-config)
(require 'uniquify-config)
(require-package 'ido-config :dependencies '(ido-completing-read+))
(require 'eww-config)
(require 'scales)
(require 'calendar-config)
(require 'dired-config)
(require 'vc-config)
;; depend on diff-hl
(require 'version-controll)
(require-package 'swiper-config :dependencies '(swiper counsel))
(require 'org-config)
(require 'gkroam-config)
(require 'slime-config)
(require-package 'projectile-config :dependencies '(projectile))
(require 'files-config)
;(require 'evil-config)
;; depend on eyebrowse
(require 'f-zone-key-bindings)
;; depend on ac-emoji, swift-mode
(require 'emoji-config)
;; download package
(require 'web-config)
(require-package 'smart-compile-config :dependencies '(smart-compile))
(require 'python-config)
(require 'lsp-config)
;; package + post config, in a seperate repo?? not in emacs.git, but should keep in cvs
(require 'youdao-dictionary-conf)
(require 'nov-config)
;; self config, require safely
(require 'pdf-config)
(require-package 'redis-config :dependencies '(eredis))
(require-package 'annotate-config :dependencies '(annotate))
(require-package 'yas-config :dependencies '(yasnippet dash ivy) (yas-global-mode))
(require 'anythings)
(require 'exwm-conf)
;; depend on eyebrowse+, async
(require-package 'frames :dependencies '(async))
;; depend on erc+
(require 'irc-config)
;; depend on telega
(require 'telega-config)

;; -------
;; network-util, rest-util, minibuffer+
;; -------
;; require el-node
(require-package 'el-server :dependencies '(elnode))
(require-package 'el-server-extend :dependencies '(elnode))
;; depend on goto-last-point
(require-package 'goto-last-point+ :dependencies '(goto-last-point))
(require-package 'notifications :dependencies '(uuid))
(require-package 'customized-dir (customized-dir-init))
(require-package 'stopwatch :dependencies '(switch-buffer-functions) (stopwatch-mode 1))
(require-package 'eyebrowse+ :dependencies '(eyebrowse) (eyebrowse-plus-mode 1))
(require-package 'eyebrowse-xmonad :dependencies '(eyebrowse s) (eyebrowse-xmonad-mode 1))
(require-package 'wcy-desktop (wcy-desktop-init))
(require-package 'password-generator+ :dependencies '(password-generator))


;; the following should move to el-post-scripts
;; package + post config
(require-package 'sudo-edit)
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
