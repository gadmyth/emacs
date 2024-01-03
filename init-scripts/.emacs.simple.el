;;; package --- .emacs.simple.el
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

;; init archives
(packages-init-archives)

;; load configs
(require 'loading-config)
(require 'basic-config)

(require 'scratch+)

(require 'minibuffer+)
;; library
(global-set-key (kbd "<f6>") (toggle-minibuffer 'find-library))
(global-set-key (kbd "<M-f6>") (toggle-minibuffer 'require-library))
(global-set-key (kbd "<C-f6>") (toggle-minibuffer 'load-library))

;; load script files at last
(load-post-script-files)

(provide '.emacs.simple)
;;; .emacs.simple.el ends here
