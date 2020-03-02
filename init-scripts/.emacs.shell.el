;;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;; evil
;(evil-mode 1)
;(define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
;(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
;(global-set-key (kbd "C-6") 'evil-buffer)

;;; ido
(ido-mode t)
(setq ido-enable-flex-matching t)

(setq backup-directory-alist (quote (("." . "~/.backups"))))
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq-default tab-width 4)
(defalias 'yes-or-no-p 'y-or-n-p)

;;; diff
(require 'diff-hl)
(global-diff-hl-mode t)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(eval-after-load "vc-hooks" '(define-key vc-prefix-map "=" 'vc-ediff))

;;; coding system
(setq current-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setenv "LC_CTYPE" "zh_CN.UTF-8")

;;; textmate
(require 'textmate)
(eval-after-load "textmate" '(add-to-list '*textmate-project-roots* ".svn"))
(textmate-mode)

;;; F zone
(setq *MAIN-BUFFER* "futuretalk_ios.txt")
(global-set-key (kbd "<f1>") '(lambda () (interactive) (switch-to-buffer *MAIN-BUFFER*)))
(global-set-key (kbd "<f2>") 'evil-buffer)
(global-set-key (kbd "<f3>") '(lambda () (interactive) (with-current-buffer (setq *MAIN-BUFFER* (buffer-name)))))
(global-set-key (kbd "<f5>") '(lambda () (interactive)
                                (let ((index (string-match "\\(.*\\)\\.\\(.\\)" (buffer-name)))
                                      (prename (match-string 1 (buffer-name)))
                                      (suffix (match-string 2 (buffer-name))))
                                  (when (equal index 0)
                                    (if (equal "m" suffix)
                                        (find-file (concatenate 'string prename ".h"))
                                      (if (equal "h" suffix)
                                          (find-file (concatenate 'string prename ".m"))))))))

