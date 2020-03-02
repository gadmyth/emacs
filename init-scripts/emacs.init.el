;;; package --- emacs.init.el
;;; Commentary:
;;; Code:

(if (version< emacs-version "24")
    (load-file (expand-file-name ".emacs.shell.el" +emacs-init-script-directory+))
  (load-file (expand-file-name ".emacs.frame.el" +emacs-init-script-directory+)))

(provide 'emacs.init)
;;; emacs.init.el ends here

