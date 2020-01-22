(when *load-slime*
  (require 'slime-autoloads)
  (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
  (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
  (setq slime-net-coding-system 'utf-8-unix)
  (setq slime-lisp-implementations `((sbcl (,*lisp-bin-path*) :coding-system utf-8-unix)))
  (setq slime-contribs '(slime-fancy))
  (global-set-key (kbd "C-c s") 'slime-selector)
  )


(provide 'slime-config)