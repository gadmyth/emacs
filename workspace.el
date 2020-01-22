(eval-when-compile (require 'cl))

(setq def-dir (expand-file-name "~/emacs-workspace"))

(if (not (file-exists-p def-dir))
    (mkdir def-dir))
(setq default-directory def-dir)

(setq *mac-scale-amount* 2
      *linux-scale-amount* 2)

(setq *load-slime* nil
	  *slime-path* "~/emacs/slime"
	  *lisp-bin-path* "/usr/local/bin/sbcl")

(setq *default-trans-value* 100)

(provide 'workspace)
