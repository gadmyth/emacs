;;; package --- irc-config.el
;;; Commentary:
;;; Code:

(require-package
 'erc+
 :dependencies
 '(erc s)

 (defvar erc-hide-list '("JOIN" "QUIT" "PART" "NICK" "MODE"))

 (setq erc-server-coding-system '(utf-8-unix . utf-8-unix))
 )

(provide 'irc-config)
;;; irc-config.el ends here
