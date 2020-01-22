;;; package --- version-controll.el
;;; Commentary:
;;; Code:


(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(eval-after-load "vc-hooks" '(define-key vc-prefix-map "=" 'vc-ediff))
(eval-after-load "vc-hooks" '(define-key vc-prefix-map "k" 
							   (lambda (&optional historic not-urgent)
								 (interactive (list current-prefix-arg t))
								 (mapc (lambda (file) (delete-file file)) (cadr (vc-deduce-fileset t))))))


(require-if-installed
 'magit
 (setq magit-auto-revert-mode nil)

 (let ((options (plist-get magit-diff-popup :options))
       (word-diff '(?w "word-diff" "--word-diff=" magit-word-diff-select)))
   (plist-put magit-diff-popup :options (cons word-diff options)))

 (defun magit-word-diff-select (&rest _ignore)
   (magit-read-char-case nil t
                         (?c "[c]olor" "color")
                         (?p "[p]lain" "plain")
                         (?L "porce[L]ain" "porcelain")))
 )
 
(when window-system
  (require 'diff-hl)
  (global-diff-hl-mode t))

(setq mode-require-final-newline nil)

(provide 'version-controll)
;;; version-controll.el ends here
