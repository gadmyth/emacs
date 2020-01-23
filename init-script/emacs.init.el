;;; package --- emacs.init.el
;;; Commentary:
;;; Code:

(if (version< emacs-version "24")
    (load-file (expand-file-name "~/.emacs.shell.el"))
  (load-file (expand-file-name "~/.emacs.frame.el")))

(provide 'emacs.init)
;;; emacs.init.el ends here

