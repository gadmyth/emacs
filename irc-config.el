(require 'erc)
(setq erc-nick "gadmyth")
(setq erc-hide-list '("JOIN" "QUIT" "PART" "NICK" "MODE"))
(defun start-erc (passwd)
  (interactive (list (read-passwd "passwd: ")))
  (erc :server erc-default-server :port erc-default-port :nick erc-nick :password passwd))
(setq erc-autojoin-channels-alist '(("freenode.net" "#wecase" "#emacs")))

(provide 'irc-config)