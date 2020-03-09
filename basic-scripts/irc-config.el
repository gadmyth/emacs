;;; package --- irc-config.el
;;; Commentary:
;;; Code:

(require 'erc)
(require 's)
(require 'text-mode)

(define-derived-mode erc-aggregate-mode text-mode "ErcA"
  ;; The mode for *erc-aggregate-buffer*.
  )

(defvar erc-aggregate-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent erc-aggregate-mode-map text-mode-map)
    (define-key map "q" 'quit-window)
    map))

(defvar erc-nick nil)
(defvar erc-hide-list '("JOIN" "QUIT" "PART" "NICK" "MODE"))
(defvar *erc-aggregate-buffer* nil)

(defface erc-time-face
  '((default :weight bold :foreground "Green")
    (t :foreground "green"))
  "ERC face for time."
  :group 'erc-faces)

(defface erc-link-face
  '((default :underline t :foreground "SlateBlue")
    (t :foreground "blue"))
  "ERC face for link."
  :group 'erc-faces)

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
         (sender (erc-sender-with-group short-sender tgt)))
    (erc-update-aggregate-buffer sender msg)
    (message "erc message: %s: %s" sender msg)
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

(defvar *wechat-multi-message-table* (make-hash-table :test 'equal))

(defun erc-parse-wechat-share-msg (sender msg)
  "Parse and store wechat multi share MSG and SENDER."
  (cond
   ((string-prefix-p "[应用分享]" msg)
    (let ((table *wechat-multi-message-table*)
          (title-key (concat sender "_title"))
          (desc-key (concat sender "_description"))
          (app-key (concat sender "_app"))
          (link-key (concat sender "_link")))
      ;; record the wechat share msg
      (cond ((s-contains-p "标题：" msg)
             (puthash title-key (substring msg 9) table))
            ((s-contains-p "描述：" msg)
             (puthash desc-key (substring msg 9) table))
            ((s-contains-p "应用：" msg)
             (puthash app-key (substring msg 9) table))
            ((s-contains-p "链接：" msg)
             (puthash link-key (substring msg 9) table)))
      ;; merge 4 msg to one
      (cond ((and (gethash title-key table)
                  (gethash desc-key table)
                  (gethash app-key table)
                  (gethash link-key table))
             (setq msg (format "[%s]\n%s\n%s"
                               (gethash title-key table)
                               (gethash desc-key table)
                               (gethash link-key table)))
             (remhash title-key table)
             (remhash desc-key table)
             (remhash app-key table)
             (remhash link-key table)
             msg)
            (t
             nil))))
   ;; msg is not a wechat share msg
   (t msg)))

(defun* erc-update-aggregate-buffer (sender msg)
  "Append SENDER and MSG to the *erc-aggregate-buffer*."
  (if-let ((wechat-msg (erc-parse-wechat-share-msg sender msg)))
      (setq msg wechat-msg)
    (return-from erc-update-aggregate-buffer))
  
  (when (not (buffer-live-p *erc-aggregate-buffer*))
    ;; create buffer if not exists
    (setq *erc-aggregate-buffer*
          (generate-new-buffer "*erc-aggregate-buffer*"))
    ;; set the major mode
    (with-current-buffer *erc-aggregate-buffer*
      (erc-aggregate-mode)))
  ;; append msg
  (with-current-buffer *erc-aggregate-buffer*
    (goto-char (point-max))
    (let* ((now (format-time-string "%Y-%m-%d %a %H:%M:%S" (current-time)))
           (content (format "%s [%s]:\n%s\n\n" sender now msg))
           (time-start (- (s-index-of now content) 1))
           (time-end (+ time-start (length now) 2))
           (msg-start (+ time-end 2))
           (msg-end (+ msg-start (length msg))))
      (erc-put-text-property 0 (length sender)
                             'font-lock-face 'erc-nick-default-face content)
      (erc-put-text-property time-start time-end
                             'font-lock-face 'erc-notice-face content)
      ;; font lock the link
      (when-let ((index (string-match "http" msg)))
        (erc-put-text-property  (+ msg-start index) msg-end
                                'font-lock-face 'erc-link-face content))
      (insert content)))
  ;; show window
  (display-buffer *erc-aggregate-buffer*))

(provide 'irc-config)
;;; irc-config.el ends here
