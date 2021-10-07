;;; package --- swiper-config.el
;;; Commentary:
;;; Code:

(require 'swiper)
(require 'counsel)
(require 'thingatpt)

(ivy-mode 1)

(setq ivy-re-builders-alist
      '((t . ivy--regex-plus)))

(setq ivy-initial-inputs-alist nil)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "%d/%d ")

(define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
(define-key swiper-map [escape] 'minibuffer-keyboard-quit)

(defun swiper-with-word-at-point ()
  "If region is activate, swiper the word in the region from START to END."
  "Otherwise, swiper the word at point, if current-prefix-arg is not null, toggle the superword-mode."
  (interactive)
  (if (and (mark) (region-active-p))
      (progn
        (deactivate-mark)
        (swiper (buffer-substring-no-properties (region-beginning) (region-end))))
    (let* ((should-toggle (not (null current-prefix-arg)))
           (origin-value (if (and (boundp 'superword-mode) superword-mode) 1 0))
           (toggle-value (if (and (boundp 'superword-mode) (not superword-mode)) 1 0)))
      (if should-toggle (superword-mode toggle-value))
      (let ((word (word-at-point t)))
        (if should-toggle (superword-mode origin-value))
        (message "should toggle: %S, current is origin-value: %S, toggle-value: %S" should-toggle origin-value toggle-value)
        (message "string-at-point is [%s]" word)
        (swiper word)))))

;(global-set-key "\C-s" 'swiper-with-word-at-point)
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
