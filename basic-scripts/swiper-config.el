;;; package --- swiper-config.el
;;; Commentary:
;;; Code:

(require 'swiper)
(require 'counsel)
(require 'ffap)

(ivy-mode 1)

(setq ivy-re-builders-alist
      '((t . ivy--regex-plus)))

(setq ivy-initial-inputs-alist nil)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "%d/%d ")

(define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
(define-key swiper-map [escape] 'minibuffer-keyboard-quit)

(defun swiper-with-symbol-at-point ()
  "."
  (interactive)
  (let* ((string-at-point (ffap-string-at-point)))
    (message "string-at-point is [%s]" string-at-point)
    (swiper string-at-point)))

(global-set-key "\C-s" 'swiper-with-symbol-at-point)
(global-set-key (kbd "C-c r") 'ivy-resume)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
;;(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
;;(global-set-key (kbd "<f1> f") 'counsel-describe-function)
;;(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;;(global-set-key (kbd "<f1> l") 'counsel-load-library)
;;(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;;(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c C-g") 'counsel-git)
(global-set-key (kbd "C-c C-u") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
;;(global-set-key (kbd "C-x l") 'counsel-locate)
;;(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;;(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

(provide 'swiper-config)
;;; swiper-config.el ends here
