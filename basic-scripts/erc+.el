;;; erc+.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2021 gadmyth

;; Author: erc+.el <gadmyth@gmail.com>
;; Version: 1.0.2
;; Package-Version: 20220609.001
;; Package-Requires: erc, s, text-mode, system-util, browse-url+
;; Keywords: erc+.el
;; Homepage: https://www.github.com/gadmyth/emacs
;; URL: https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/erc+.el

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Source code
;;
;; erc+'s code can be found here:
;;   https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/erc+.el

;;; Commentary:
;;; Code:

(require 'erc)
(require 's)
(require 'text-mode)
(require 'system-util)
(require 'browse-url+)


(defvar erc-nick nil)
(defvar *erc-debug* nil)
(defvar *erc-aggregate-buffer* nil)
(defvar erc-aggregate-refresh t)
(defvar erc-aggregate-auto-display nil)
(defvar erc-default-width 100)
(defvar *erc-forbidden-targets* nil)
(defvar *erc-forbidden-targets-unread-count* nil)
(defconst *erc-forbidden-targets-file-name* (expand-file-name "~/.erc_forbidden_targets"))
(defconst *erc-forbidden-targets-actions* '(("jump" . erc-jump-to-buffer)
                                            ("reset unread count" . erc-reset-forbidden-taget-unread-count)
                                            ("unforbidden" . erc-unforbidden-target)))

(defun erc-toggle-debug ()
  "."
  (interactive)
  (setq *erc-debug* (not *erc-debug*))
  (message "turn %s the *erc-debug*" (if *erc-debug* "on" "off")))

(defmacro erc-debug-message (format-string &rest ARGS)
  "If debug is open, send message with FORMAT-STRING and ARGS."
  `(if *erc-debug*
       (message ,format-string ,@ARGS)))

(defun erc-aggregate-toggle-refresh ()
  "."
  (interactive)
  (setq erc-aggregate-refresh (not erc-aggregate-refresh))
  (message "turn %s the erc refresh." (if erc-aggregate-refresh "on" "off")))

(defun erc-aggregate-toggle-auto-display ()
  "."
  (interactive)
  (setq erc-aggregate-auto-display (not erc-aggregate-auto-display))
  (message "turn %s the erc auto display." (if erc-aggregate-auto-display "on" "off")))

;; **** add a extra function to erc-server-PRIVMSG-functions hook ****
(add-hook 'erc-server-PRIVMSG-functions #'erc-server-PRIVMSG_AGGREGATE t)

(defun erc-server-PRIVMSG_AGGREGATE (proc parsed)
  "Handle private messages, including messages in channels.
With PARSED message and PROC."
  (let* ((sender-spec (erc-response.sender parsed))
         (cmd (erc-response.command parsed))
         (tgt (car (erc-response.command-args parsed)))
         (msg (erc-response.contents parsed))
         (short-sender (erc-short-sender sender-spec)))
    (erc-debug-message "erc message: %s, %s: %s" short-sender tgt msg)
    ;; load forbidden targets
    (when (not (buffer-live-p *erc-aggregate-buffer*))
      ;; create buffer if not exists
      (setq *erc-aggregate-buffer*
            (generate-new-buffer "*erc-aggregate-buffer*"))
      (load-erc-forbidden-targets)
      ;; set the major mode
      (with-current-buffer *erc-aggregate-buffer*
        (erc-aggregate-mode)))
    ;; forbidden or update
    (cond ((not (member tgt *erc-forbidden-targets*))
           (erc-update-aggregate-buffer short-sender tgt msg))
          ((get-buffer-window tgt)
           (erc-reset-forbidden-taget-unread-count tgt))
          (t
           (erc-increase-forbidden-target-unread-count tgt)))
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
    (format "%s->%s" sender target)))


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

(defun erc-parse-wechat-file-msg (msg)
  "Parse MSG if it's an file message."
  (when (string-match "\\[\\(.*\\)\\](\\(.*\\))" msg)
    (let ((indicator
           (substring msg (match-beginning 1) (match-end 1)))
          (file-path
           (substring msg (match-beginning 2) (match-end 2))))
      (cons indicator file-path))))

(defmacro erc-save-excursion (&rest body)
  "Eval the BODY using 'save-excursion or not by SAVE-EXCURSION-P."
  `(cond ((eval erc-aggregate-refresh)
          (erc-debug-message "no save excursion")
          ,@body)
         (t
          (save-excursion
            (erc-debug-message "save excursion")
            ,@body))))

(defun erc-update-aggregate-buffer (short-sender target msg)
  "Append SENDER and MSG to the *erc-aggregate-buffer*."
  (let ((sender (erc-sender-with-group short-sender target))
        erc-msg-link erc-link-start erc-link-end
        msg-file-path msg-picture-p)
    (if-let ((wechat-msg (erc-parse-wechat-share-msg sender msg)))
        (setq msg wechat-msg)
      (cl-return-from erc-update-aggregate-buffer))

    ;; parse the wechat file msg as picture, file or emoji
    (when-let ((file-msg (erc-parse-wechat-file-msg msg)))
      (setq msg (car file-msg))
      (setq msg-file-path (cdr file-msg))
      (when (or (string-equal "图片" (car file-msg))
                (string-equal "表情" (car file-msg)))
        (setq msg "")
        (setq msg-picture-p t)))

    ;; parse the link
    (when-let ((index (string-match "\\(https?://[^\s]*\\)" msg)))
      (setq erc-link-start index)
      (setq erc-msg-link (substring msg (match-beginning 1) (match-end 1)))
      (setq msg (concat (substring msg 0 (match-beginning 1)) "链接" (substring msg (match-end 1))))
      (setq erc-link-end (+ erc-link-start 2)))
    
    ;; append msg
    (with-current-buffer *erc-aggregate-buffer*
      (read-only-mode 0)
      (erc-save-excursion
       (goto-char (point-max))
       (let* ((now (format-time-string "%Y-%m-%d %a %H:%M:%S" (current-time)))
              (content (format "%s [%s]:\n%s\n" sender now msg))
              (new-msg-start (point-max))
              (time-start (- (s-index-of now content) 1))
              (time-end (+ time-start (length now) 2))
              (msg-start (+ time-end 2))
              (msg-end (+ msg-start (length msg))))
         ;; propertize the time part
         (erc-put-text-property time-start time-end
                                'font-lock-face 'erc-notice-face content)
         
         (insert content)
         ;; insert nick button
         (erc-button-add-button new-msg-start
                                (+ new-msg-start (length sender))
                                #'erc-button-nick-callback
                                nil (list (cons short-sender target)))
         ;; insert file or picture
         (when msg-file-path
           (cond (msg-picture-p
                  (let ((image (create-image msg-file-path)))
                    (image-flush image)
                    (when (image-type-available-p 'imagemagick)
                      (plist-put (cdr image) :type 'imagemagick))
                    (plist-put (cdr image) :width erc-default-width)
                    (insert-image image))
                  (insert "\n"))
                 (t
                  (erc-button-add-button (+ new-msg-start msg-start)
                                         (+ new-msg-start msg-end)
                                         #'erc-button-file-callback
                                         nil (list msg-file-path)))))
         ;; insert link button
         (when erc-msg-link
           (erc-button-add-button (+ new-msg-start msg-start erc-link-start)
                                  (+ new-msg-start msg-start erc-link-end)
                                  #'erc-button-link-callback
                                  nil (list erc-msg-link)))
         (insert "\n")))
      (read-only-mode t))
    ;; show window
    (when erc-aggregate-auto-display
      (display-buffer *erc-aggregate-buffer*))))


(defvar *erc-nick-function-list* '(("jump  chat" . erc-jump-target)
                                   ("reply chat" . erc-reply-message)))

(defun erc-button-nick-callback (data)
  "When click the nick name erc-button, response with the DATA."
  (let* ((action-list *erc-nick-function-list*)
         (action (intern (completing-read "Select the action: "
                                          action-list nil t)))
         (func (alist-get action action-list nil nil #'string=)))
    (funcall func data)))

(defun erc-jump-target (data)
  "Jump to target's buffer when the target info in the DATA."
  (let* ((short-sender (car data))
         (target (cdr data))
         (channel (and (string-prefix-p "#" target) target))
         (buffer-name (or channel
                          (and (string-equal target erc-nick) short-sender)
                          target)))
    (erc-debug-message "nick click, data: %s, short-sender: %s, channel: %s, buffer-name: %s" data short-sender channel buffer-name)
    (erc-jump-to-buffer buffer-name)))

(defun erc-jump-to-buffer (buffer-name)
  "Jump to the buffer of BUFFER-NAME."
  (interactive)
  (let* ((buffer (get-buffer buffer-name))
         (window (get-buffer-window buffer)))
    (when (assoc (intern buffer-name) *erc-forbidden-targets-unread-count*)
      (erc-reset-forbidden-taget-unread-count buffer-name))
    (when buffer
      (display-buffer buffer)
      (if (and window (window-live-p window))
          (select-window window)))))


(defun erc-reply-message (data)
  "Reply message to target with content which are wrapped in DATA."
  (let* ((short-sender (car data))
         (target (cdr data))
         (channel (and (string-prefix-p "#" target) target))
         (buffer-name (or channel
                          (and (string-equal target erc-nick) short-sender)
                          target))
         (content (completing-read (format "Reply the chat %s: " buffer-name) nil nil t)))
    (with-current-buffer (get-buffer buffer-name)
      (goto-char (point-max))
      (insert content)
      (erc-send-current-line))))

(defun erc-button-file-callback (data)
  "When click the nick name erc-button, response with the DATA."
  ;; file(open-file), picture(show)
  (message "file path: %s" data)
  (if (not (fboundp 'open-file-by-system))
      (require 'system-util))
  (apply #'open-file-by-system (list data)))

(defvar *erc-image-action-list* '(erc-ffap-image erc-open-image erc-image-path))

(defun erc-button-link-callback (data)
  "When click the nick name erc-button, response with the DATA to open the link."
  (browse-url-select-function data))

(defun erc-image-at-point ()
  "Get the image at point."
  (interactive)
  (let ((image (get-text-property (point) 'display)))
    (if (eq 'image (car image)) image nil)))

(defun erc-open-image (&rest image)
  "Open the IMAGE by it's path."
  (if-let ((image-path (plist-get (cdr image) :file)))
      (open-file-by-system image-path)))

(defun erc-image-path (&rest image)
  "Open the IMAGE by it's path."
  (when-let ((image-path (plist-get (cdr image) :file)))
    (message "image path is: %s" image-path)
    image-path))

(defun erc-ffap-image (&rest image)
  "Open the IMAGE by it's path."
  (if-let ((image-path (plist-get (cdr image) :file)))
      (ffap image-path)))

(defun erc-right-click ()
  "Double click in the erc aggregate buffer."
  (interactive)
  (if-let ((image (erc-image-at-point)))
      (let* ((action-list *erc-image-action-list*)
             (action (intern (completing-read "Select the action: "
                                              action-list nil t))))
        (apply action image))))

(defun erc-aggregate-nick-name-p ()
  "Check the current point is a nick name button or not."
  (eq 'erc-button-nick-callback
      (get-text-property (point) 'erc-callback)))

(defun erc-aggregate-current-message ()
  "Goto the current message's nick name head in *erc-aggregate-buffer*."
  (interactive)
  (cl-loop while (and (not (erc-aggregate-nick-name-p))
                   (not (eq (point) (point-min)))
                   (not (eq (point) (point-max))))
        do (previous-line)))

(defun erc-aggregate-previous-message ()
  "Goto the previous message's nick name head in *erc-aggregate-buffer*."
  (interactive)
  (previous-line)
  (cl-loop while (and (not (erc-aggregate-nick-name-p))
                   (not (eq (point) (point-min)))
                   (not (eq (point) (point-max))))
        do (previous-line)))

(defun erc-aggregate-next-message ()
  "Goto the next message's nick name head in *erc-aggregate-buffer*."
  (interactive)
  (forward-line)
  (cl-loop while (and (not (erc-aggregate-nick-name-p))
                   (not (eq (point) (point-min)))
                   (not (eq (point) (point-max))))
        do (forward-line)))

(defun erc-jump-this-message ()
  "Jump to the chat of this current message."
  (interactive)
  (save-excursion
    (erc-aggregate-current-message)
    (let ((data (car (get-text-property (point) 'erc-data))))
      (erc-jump-target data))))

(defun erc-reply-this-message ()
  "Reply the chat of this current message."
  (interactive)
  (save-excursion
    (erc-aggregate-current-message)
    (let ((data (car (get-text-property (point) 'erc-data))))
      (erc-reply-message data))))

(defun erc-delete-this-message ()
  "Delete the message in *erc-aggregate-buffer*."
  (interactive)
  (read-only-mode 0)
  (erc-aggregate-current-message)
  (let ((msg-start (point)))
    (erc-aggregate-next-message)
    (let ((msg-end (point)))
      (delete-region msg-start msg-end)
      (erc-debug-message "erc message deleted from %S to %S"  msg-start msg-end)))
  (read-only-mode 1))

(defun erc-delete-all-channel-message ()
  "Delete all the channel's message in *erc-aggregate-buffer*."
  (interactive)
  (save-excursion
    (erc-aggregate-current-message)
    (let* ((data (car (get-text-property (point) 'erc-data)))
           (short-sender (car data))
           (target (cdr data))
           (channel (and (string-prefix-p "#" target) target))
           (chat (and (not channel)
                      (or (and (string-equal target erc-nick) short-sender) target))))
      (if channel
          (erc-debug-message "delete all the channel [%S]'s message" channel)
        (erc-debug-message "delete all the chat [%S]'s message" chat))
      (when (or channel chat)
        (goto-char (point-min))
        (let ((data (erc-message-data)))
          (while data
            (erc-debug-message "get the message data: [%S], channel: [%S], chat: [%S]" data channel chat)
            (if (or (and channel (erc-message-channel-p data channel))
                    (erc-message-chat-p data chat))
                (progn
                  (if channel
                      (erc-debug-message "the message [%S] is matched the channel: %S" data channel)
                    (erc-debug-message "the message [%S] is matched the chat: %S" data chat))
                  (erc-delete-this-message))
              (erc-aggregate-next-message))
            (setq data (erc-message-data))))))))

(defun erc-aggregate-undo ()
  "Undo in the *erc-aggregate-buffer*."
  (interactive)
  (read-only-mode 0)
  (undo)
  (read-only-mode 1))

(defun erc-message-data ()
  "."
  (let* ((data (car (get-text-property (point) 'erc-data))))
    data))

(defun erc-message-chat-p (message-data chat)
  "Check the message with MESSAGE-DATA blongs to the CHAT."
  (let* ((short-sender (car message-data))
         (target (cdr message-data))
         (channel (and (string-prefix-p "#" target) target))
         (the-chat (and (not channel)
                        (or (and (string-equal target erc-nick) short-sender)
                            target))))
    (erc-debug-message "chat: %S, the-chat: %S" chat the-chat)
    (and chat (string-equal the-chat chat))))

(defun erc-message-channel-p (message-data channel)
  "Check the message with MESSAGE-DATA blongs to the CHANNEL."
  (let* ((target (cdr message-data))
         (the-channel (and (string-prefix-p "#" target) target)))
    (erc-debug-message "channel: %S, the-channel: %S" channel the-channel)
    (and channel (string-equal channel the-channel))))

(defun erc-forbidden-this-channel ()
  "Forbidden the channel at this point in *erc-aggregate-buffer*."
  (interactive)
  (save-excursion
    (erc-aggregate-current-message)
    (let* ((data (car (get-text-property (point) 'erc-data)))
           (target (cdr data))
           (channel (and (string-prefix-p "#" target) target)))
      (when channel
        (add-to-list '*erc-forbidden-targets* channel)
        (message "The channel %s is forbbiden now." channel)))))

(defun erc-unforbidden-this-channel ()
  "Unforbidden the channel at this point in *erc-aggregate-buffer*."
  (interactive)
  (save-excursion
    (erc-aggregate-current-message)
    (let* ((data (car (get-text-property (point) 'erc-data)))
           (target (cdr data))
           (channel (and (string-prefix-p "#" target) target)))
      (when channel
        (setq *erc-forbidden-targets* (delete channel *erc-forbidden-targets*))
        (message "The channel %s is unforbbiden now." channel)))))

(defun erc-increase-forbidden-target-unread-count (target)
  "Increase the forbidden TARGET's unread message count."
  (when target
    (let* ((count (cdr (assoc (intern target) *erc-forbidden-targets-unread-count*)))
           (count (if count (+ 1 count) 1)))
      (setf (alist-get (intern target) *erc-forbidden-targets-unread-count*) count))))

(defun erc-reset-forbidden-taget-unread-count (target)
  "Reset the forbidden TARGET's unread message count to zero."
  (when target
    (let ((count (cdr (assoc (intern target) *erc-forbidden-targets-unread-count*))))
      (when (and count (> count 0))
        (setf (alist-get (intern target) *erc-forbidden-targets-unread-count*) 0)))))

(defun erc-unforbidden-target (target)
  "Unforbidden the TARGET in *erc-aggregate-buffer*."
  (interactive)
  (when-let ((channel target))
    (setq *erc-forbidden-targets* (delete channel *erc-forbidden-targets*))
    (message "The channel %s is unforbbiden now." channel)))

(defun erc-action-on-forbidden-target ()
  "Unforbidden the channel at this point in *erc-aggregate-buffer*."
  (interactive)
  (let* ((channel (completing-read "Choose the channel: "
                                   (mapcar (lambda (target)
                                             (let ((unread-count (cdr (assoc (intern target) *erc-forbidden-targets-unread-count*))))
                                               (if (and unread-count (> unread-count 0))
                                                   (format "%s (%d)" target unread-count)
                                                 target)))
                                           *erc-forbidden-targets*)
                                   nil t nil nil nil))
         (channel (replace-regexp-in-string " (.*)" "" channel))
         (action (completing-read (format "Choose the action for the channel %s:" channel)
                                  *erc-forbidden-targets-actions* nil t nil nil nil))
         (fn (cdr (assoc action *erc-forbidden-targets-actions*))))
    (when (and channel fn)
      (funcall fn channel))))

(defun load-erc-forbidden-targets ()
  "Load *erc-forbidden-targets* from file."
  (interactive)
  (let ((file-name *erc-forbidden-targets-file-name*))
    (cond
     ((not (file-exists-p file-name))
      (message "Can't load %s file, for it does not exist!" file-name))
     (t
      (message "Loading *erc-forbidden-targets* from file %S ..." file-name)
      (with-temp-buffer
        (insert-file-contents file-name)
        (goto-char (point-min))
        (let ((content (read (current-buffer))))
          (message "content: %S" content)
          (setq *erc-forbidden-targets* content)))))))

(defun save-erc-forbidden-targets ()
  "Save *erc-forbidden-targets* to file."
  (interactive)
  (let ((file-name *erc-forbidden-targets-file-name*))
    (message "Saving *erc-forbidden-targets* to file %S ..." file-name)
    (let ((content *erc-forbidden-targets*))
      (with-temp-file file-name
        (print content (current-buffer))))))

(defun delete-erc-forbidden-targets (target)
  "Delete a TARGET from *erc-forbidden-targets*."
  (interactive)
  (message "Deleting the target %s from *erc-forbidden-targets* ..." target)
  (setq *erc-forbidden-targets* (delete target *erc-forbidden-targets*))
  (message "The target %s is deleted now." target))

(defvar erc-aggregate-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map "n" 'erc-aggregate-next-message)
    (define-key map "p" 'erc-aggregate-previous-message)
    (define-key map "j" 'erc-jump-this-message)
    (define-key map "r" 'erc-reply-this-message)
    (define-key map "d" 'erc-delete-this-message)
    (define-key map "D" 'erc-delete-all-channel-message)
    (define-key map "f" 'erc-forbidden-this-channel)
    (define-key map "F" 'erc-unforbidden-this-channel)
    (define-key map "U" 'erc-action-on-forbidden-target)
    (define-key map "u" 'erc-aggregate-undo)
    (define-key map "q" 'quit-window)
    (define-key map (kbd "<mouse-3>") 'erc-right-click)
    map))

(define-derived-mode erc-aggregate-mode text-mode "ErcA"
  ;; The mode for *erc-aggregate-buffer*.
  (use-local-map erc-aggregate-mode-map))

(provide 'erc+)
;;; erc+.el ends here
