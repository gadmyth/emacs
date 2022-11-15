;;; package --- swiper-config.el
;;; Commentary:
;;; Code:

(require 'swiper)
(require 'counsel)
(require 'thingatpt)
(require 'ffap)

(ivy-mode 1)

(setq ivy-re-builders-alist
      '((t . ivy--regex-plus)))

(setq ivy-initial-inputs-alist nil)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "%d/%d ")

(define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
(define-key swiper-map [escape] 'minibuffer-keyboard-quit)

(defun counsel-grep-with-word-at-point ()
  "If region is activate, counsel-grep the word in the region from START to END."
  "Otherwise, counsel-grep the word at point, if current-prefix-arg is not null, toggle the superword-mode."
  (interactive)
  (let* ((file-name (buffer-file-name (current-buffer)))
         func
         (word (region-or-word-at-point)))
    ;; determine the func
    (cond
     ((not file-name)
      (setq func #'swiper))
     (t
      (setq func #'counsel-grep)
      ;; determine the counsel-grep-base-command
      (cond
       ((equal ".gz" (ffap-file-suffix file-name))
        (setq counsel-grep-base-command "zgrep -E -n -e %s %s"))
       (t
        (setq counsel-grep-base-command "grep -E -n -e %s %s")))))
    ;; execute the func
    (funcall func word)))

(defun region-or-word-at-point ()
  "."
  (let (word)
    (cond
     ((region-active-p)
      (setq word (buffer-substring-no-properties (region-beginning) (region-end)))
      (message "string-at-region is [%s]" word)
      (deactivate-mark))
     (t
      (let* ((should-toggle (not (null current-prefix-arg)))
             (origin-value (if (and (boundp 'superword-mode) superword-mode) 1 0))
             (toggle-value (if (and (boundp 'superword-mode) (not superword-mode)) 1 0)))
        (if should-toggle (superword-mode toggle-value))
        (setq word (word-at-point t))
        (if should-toggle (superword-mode origin-value))
        (message "should toggle: %S, current is origin-value: %S, toggle-value: %S"
                 should-toggle origin-value toggle-value)
        (message "string-at-point is [%s]" word))))
    word))


(defun counsel-git-with-word-at-point ()
  "."
  (interactive)
  (let ((word (region-or-word-at-point)))
    (funcall 'counsel-git word)))

(defun counsel-git-grep-with-word-at-point ()
  "."
  (interactive)
  (let ((word (region-or-word-at-point)))
    (funcall 'counsel-git-grep word)))

(defun counsel-ag-with-word-at-point ()
  "."
  (interactive)
  (let ((word (region-or-word-at-point)))
    (funcall 'counsel-ag word)))

(global-set-key (kbd "C-S-s") 'counsel-grep-with-word-at-point)
(global-set-key (kbd "C-c r") 'ivy-resume)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
;;(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
;;(global-set-key (kbd "<f1> f") 'counsel-describe-function)
;;(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;;(global-set-key (kbd "<f1> l") 'counsel-load-library)
;;(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;;(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g f") 'counsel-git-with-word-at-point)
(global-set-key (kbd "C-c g g") 'counsel-git-grep-with-word-at-point)
(global-set-key (kbd "C-c g k") 'counsel-ag-with-word-at-point)
;;(global-set-key (kbd "C-x l") 'counsel-locate)
;;(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;;(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

(provide 'swiper-config)
;;; swiper-config.el ends here
