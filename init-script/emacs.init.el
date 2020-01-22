(if (string< (emacs-version) "24")
    (load-file (expand-file-name "~/.emacs.shell.el"))
  (load-file (expand-file-name "~/.emacs.frame.el")))
