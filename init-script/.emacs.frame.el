;;; package --- .emacs.frame.el
;;; Commentary:
;;; Code:
(defvar ts-init (current-time))

;; load path
(add-to-list 'load-path (expand-file-name "~/emacs"))
(add-to-list 'load-path (expand-file-name "el-pre-scripts" "~/emacs"))
(add-to-list 'load-path (expand-file-name "el-extends" "~/emacs"))

;; load script files at first
(require 'script-extends)
(load-pre-script-files)

;; insall packages
(require 'package-tools)
(require 'packages)
(install-packages-if-needed +required-packages+)

;; load configs
(require 'basic-config)
(require 'utility)
(require 'frames)
(require 'windows)
(require 'mode-bars)
(require 'fonts)
(require 'themes)
(require 'shells)
(require 'irc-config)
(require 'tab-config)
(require 'clipboard)
(require 'codings)
(require 'codec)
(require 'dates)
(require 'abbrev-config)
(require 'anythings)
(require 'uniquify-config)
(require 'workspace)
(require 'version-controll)
(require 'swiper-config)
(require 'lisping-snippet)
(require 'org-config)
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
(require 'eyebrowse-config)
(require 'key-bindings)
(require 'holiday-config)
(require 'dired-config)
;(require 'annot)
(require 'emoji-config)
(require 'sudo-edit)
(require 'web-config)
(require 'smart-compile-config)
(require 'frames)
(require 'python-config)

(require-if-installed 'eredis (require 'redis-config))
(require-if-installed 'smartparens (smartparens-global-mode))
(require-if-installed 'expand-region (global-set-key (kbd "C-=") 'er/expand-region))
(require-if-installed 'alpha (transparency-set-value *default-trans-value*))
(require-if-installed 'smex (global-set-key (kbd "M-x") 'smex))
(require-package 'yas-config (yas-global-mode))
(require-package 'auto-complete (global-auto-complete-mode))
(require-package 'customized-dir (customized-dir-init))
(require-package 'wcy-desktop (wcy-desktop-init))

(eval-after-load "textmate" '(add-to-list '*textmate-project-roots* ".svn"))
(eval-after-load "xcscope" '(ignore-errors (add-to-list 'cscope-indexer-suffixes "*.java")))
(switch-proxy nil)

;; load must files at last
(load-must-files)

;; load script files at last
(load-extend-script-files)

;; maximize the frame
(toggle-frame-maximized)
(message "end : %.2f" (float-time (time-since ts-init)))

(provide '.emacs.frame)
;;; .emacs.frame.el ends here
