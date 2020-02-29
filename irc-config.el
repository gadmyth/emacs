;;; package --- irc-config.el
;;; Commentary:
;;; Code:

(require 'erc)
(require 's)

(defvar erc-nick nil)
(defvar erc-hide-list '("JOIN" "QUIT" "PART" "NICK" "MODE"))
(defvar *erc-aggregate-buffer* nil)

(defun start-erc (server port nick-name passwd)
  "Start erc with NICK-NAME and PASSWD of erc SERVER at PORT."
  (interactive (list
                (read-string "server: " erc-default-server)
                (read-number "port: " erc-default-port)
                (read-string "nick name: " erc-nick)
                (read-passwd "passwd: ")))
  (erc :server server :port port :nick nick-name :password passwd))

;; **** define the autojoin channels ****
;(require 'erc-join)
;(setq erc-autojoin-channels-alist '(("freenode.net" "#wecase" "#emacs")))


;; **** add a extra function to erc-server-PRIVMSG-functions hook ****
(add-hook 'erc-server-PRIVMSG-functions #'erc-server-PRIVMSG_AGGREGATE t)

(defun erc-server-PRIVMSG_AGGREGATE (proc parsed)
  "Handle private messages, including messages in channels.
With PARSED message and PROC."
  (let* ((sender-spec (erc-response.sender parsed))
         (cmd (erc-response.command parsed))
         (tgt (car (erc-response.command-args parsed)))
         (msg (erc-response.contents parsed))
         (short-sender (erc-short-sender sender-spec))
         (sender (erc-sender-with-group short-sender tgt))
         (short-msg (format "%s: %s" sender msg)))
    (message "erc message: %s" short-msg)
    (update-erc-aggregate-buffer short-msg)
    ;; return nil, to exec the next function in the hook's list
    nil))

(defun erc-short-sender (sender)
  "Get short sender of the SENDER of erc-response.sender."
  (if-let ((index (s-index-of "!" sender)))
      (substring sender 0 index)
    sender))

(defun erc-sender-with-group (sender target)
  "Get sender and group of the SENDER and TARGET."
  (if (string-prefix-p "#" target)
      (let ((group-name (substring tgt 1 (length target))))
        (format "%s@%s" sender group-name))
    sender))

(defun update-erc-aggregate-buffer (msg)
  "Append MSG to the *erc-aggregate-buffer*."
  (if (not (buffer-live-p *erc-aggregate-buffer*))
      (setq *erc-aggregate-buffer*
            (generate-new-buffer "*erc-aggregate-buffer*")))
  ;; append msg
  (with-current-buffer *erc-aggregate-buffer*
    (goto-char (point-max))
    (insert msg)
    (newline))
  ;; show window
  (display-buffer *erc-aggregate-buffer*))
  

(provide 'irc-config)
;;; irc-config.el ends here
