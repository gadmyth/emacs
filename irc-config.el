;;; package --- irc-config.el
;;; Commentary:
;;; Code:

(require 'erc)

;; TODO: erc-nick hard code
(setq erc-nick "gadmyth")
(setq erc-hide-list '("JOIN" "QUIT" "PART" "NICK" "MODE"))

(defun start-erc (passwd)
  "Start erc with PASSWD."
  (interactive (list (read-passwd "passwd: ")))
  (erc :server erc-default-server :port erc-default-port :nick erc-nick :password passwd))

;; TODO: the follow symbol is not defined in package erc
(setq erc-autojoin-channels-alist '(("freenode.net" "#wecase" "#emacs")))

(provide 'irc-config)
;;; irc-config.el ends here
