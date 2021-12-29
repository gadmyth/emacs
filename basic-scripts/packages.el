
;;; package --- packages.el
;;; Commentary:
;;; Code:

(require 'package)

(setq package-archives nil)
(defvar package-archives-host "http://elpa.emacs-china.org")

(set-pair-to-alist 'package-archives "gnu" (concat package-archives-host "/gnu/"))
(set-pair-to-alist 'package-archives "org" (concat package-archives-host "/org/"))
(set-pair-to-alist 'package-archives "melpa" (concat package-archives-host "/melpa/"))
(set-pair-to-alist 'package-archives "melpa-stable" (concat package-archives-host "/stable-melpa/"))

;; the slime should git clone from github
(add-to-list 'load-path (expand-file-name "elpa/slime" user-emacs-directory))

(defconst +required-packages+
  (list 'try
        'xcscope
        'valign
        'async
        'dired-narrow
        'window-numbering
        'smex
        'swiper
        'counsel
        'elnode
        'sudo-edit
        'smart-compile
        'restclient
        'ob-restclient
        'wgrep
        'wgrep-ag
        'htmlize ;; org-export
        'diff-hl
        'auto-complete
        ;'anything
        'yasnippet
        'smartparens
        'multi-term
        'flycheck
        'eyebrowse
        'annotate
        'goto-last-point
        'request
        'exwm
        ;'vkill
        ))

(defconst +tool-packages+
  (list 'org-jira
        'emacs-edbi
        'ace-jump-mode
        'sos
        'ov
        'narrow-indirect
        'multifiles
        'lice
        'cal-china-x
        'eredis
        'dirtree
        'foreign-regexp
        'expand-region
        'edit-list
        'projectile
        'js2-mode
        'js2-refactor
        'web-mode
        'emmet-mode
        'gnuplot
        'urlenc
        'url-shortener
        'json-reformat
        'ac-emoji
        'google-translate
        'jq-mode
        'look-mode
        'youdao-dictionary
        'nov))

(defconst +option-packages+
  (list 'alpha
        'textmate
        'elisp-format
        'with-namespace
        'vcomp
        'elisp-sandbox
        'slime
        'magit
        'git-timemachine
        'datetime-format
        'solarized-theme
        'password-generator
        'telega
        ))

;; create autoloads
;(require 'autoload)
;(setq generated-autoload-file "~/emacs/autoloads.el")
;(update-directory-autoloads "~/emacs")
;(kill-buffer "autoloads.el")
;(require 'autoloads)


(provide 'packages)
;;; packages.el ends here
