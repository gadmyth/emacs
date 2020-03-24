;;; package --- irc-config.el
;;; Commentary:
;;; Code:

(require 'erc+)

(defvar erc-hide-list '("JOIN" "QUIT" "PART" "NICK" "MODE"))

(setq erc-server-coding-system '(utf-8-unix . utf-8-unix))

(defun erc-setup-buffer (buffer)
  "Override the erc-setup-buffer function, don't display the BUFFER automatically.")

(defun start-erc (server port nick-name passwd)
  "Start erc with NICK-NAME and PASSWD of erc SERVER at PORT."
  (interactive (list
                (read-string "server: " erc-default-server)
                (read-number "port: " erc-default-port)
                (read-string "nick name: " erc-nick)
                (read-passwd "passwd: ")))
  (erc :server server :port port :nick nick-name :password passwd))

(provide 'irc-config)
;;; irc-config.el ends here
