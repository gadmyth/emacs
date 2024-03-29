;;; p2p-websocket.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2020 gadmyth

;; Author: p2p-websocket.el <gadmyth@gmail.com>
;; Version: 0.2.6.0
;; Package-Version: 20220424.001
;; Package-Requires: websocket, s, dired-x
;; Keywords: p2p-websocket.el
;; Homepage: https://www.github.com/gadmyth/emacs
;; URL: https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/p2p-websocket.el

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
;; p2p-websocket's code can be found here:
;;   https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/p2p-websocket.el

;;; Commentary:
;;; Code:

(require 'websocket)
(require 'dired-x)

(defvar *p2p-ws-debug* nil)
(defvar *p2p-ws-server* nil)
(defvar *p2p-ws-server-host* "0.0.0.0")
(defvar *p2p-ws-server-port* 3618)
(defvar *p2p-ws-client-list* '())
(defvar *p2p-ws-nickname-list* '())

(defvar *p2p-websocket-buffer* nil)

;; -*- websocket server -*-

(defun p2p-ws-toggle-debug ()
  "."
  (interactive)
  (setq *p2p-ws-debug* (not *p2p-ws-debug*))
  (message "turn %s the *p2p-ws-debug*" (if *p2p-ws-debug* "on" "off")))

(defmacro p2p-ws-debug-message (format-string &rest ARGS)
  "If debug is open, send message with FORMAT-STRING and ARGS."
  `(if *p2p-ws-debug*
       (message ,format-string ,@ARGS)))

(defun start-websocket-server ()
  "Start the websocket server."
  (interactive)
  (setq *p2p-ws-server*
        (websocket-server
         *p2p-ws-server-port*
         :host *p2p-ws-server-host*
         :on-message #'websocket-server-message-handler
         :on-open #'websocket-server-open-handler
         :on-close #'websocket-server-close-handler)))

(defun stop-websocket-server ()
  "Close the websocket server."
  (interactive)
  (websocket-server-close *p2p-ws-server*))

(defun parse-multi-Byte-integer (string start)
  "Parse two Byte integer value of STRING from START index."
  (let* ((hh (aref string start))
         (hl (aref string (+ 1 start)))
         (lh (aref string (+ 2 start)))
         (ll (aref string (+ 3 start)))
         (value (+ (ash hh 21) (ash hl 14) (ash lh 7) ll)))
    (p2p-ws-debug-message "start: %S, [%d,%d,%d,%d], value: %S" start hh hl lh ll value)
    value))

(defun websocket-handle-text-message (compound-message)
  "Parse COMPOUND-MESSAGE as a text message."
  (let* ((index 1)
         (length (parse-multi-Byte-integer compound-message index))
         (index (+ index 4))
         (message (substring compound-message index (+ index length)))
         (message (decode-coding-string message 'utf-8)))
    (cond
     ((equal "ping" message)
      (p2p-update-websocket-buffer ws message)
      (p2p-ws-do-send-text ws "pong" t))
     ((equal "hello" message)
      (p2p-update-websocket-buffer ws message)
      (p2p-ws-do-send-text ws "world" t))
     (t
      (p2p-update-websocket-buffer ws message)))))

(defun websocket-handle-buffer-message (compound-message)
  "Parse COMPOUND-MESSAGE as a buffer message."
  (let* ((index 1)
         (buffer-name-length (parse-multi-Byte-integer compound-message index))
         (index (+ index 4))
         (buffer-name (substring compound-message index (+ index buffer-name-length)))
         (index (+ index buffer-name-length))
         (content-length (parse-multi-Byte-integer compound-message index))
         (index (+ index 4))
         (content (substring compound-message index (+ index content-length)))
         (content (decode-coding-string content 'utf-8)))
    (p2p-ws-debug-message "buffer-name: %s, content: %s" buffer-name content)
    (let* ((timestamp-string (number-to-string (time-convert nil 'integer)))
           (buffer-name (format "%s_%s" buffer-name timestamp-string)))
      (with-current-buffer (get-buffer-create buffer-name)
        (insert content))
      (let ((rich-message (format "%s" `((msg-type . buffer) (buffer-name . ,buffer-name)))))
        (p2p-update-websocket-buffer ws rich-message)))))

(defun websocket-handle-file-message (compound-message)
  "Parse COMPOUND-MESSAGE as a file message."
  (let* ((index 1)
         (sender-p (= (aref compound-message index) 1)))
    (p2p-ws-debug-message "sender-p: %S" sender-p)
    (cond
     (sender-p (websocket-handle-file-message-to-save compound-message))
     (t (websocket-handle-file-message-to-send compound-message)))))

(defun websocket-handle-file-message-to-save (compound-message)
  "Parse COMPOUND-MESSAGE as a file message, save file part."
  (let* (;; epoch
         (index 2)
         (epoch (parse-multi-Byte-integer compound-message index))
         (index (+ index 4))
         ;; saved-file-length-for-check
         (saved-file-length-for-check (parse-multi-Byte-integer compound-message index))
         (index (+ index 4))
         ;; file-path-length
         (file-path-length (parse-multi-Byte-integer compound-message index))
         (index (+ index 4))
         ;; saved-file-path-length
         (saved-file-path-length (parse-multi-Byte-integer compound-message index))
         (index (+ index 4))
         ;; file-coding-system-length
         (file-coding-system-length (parse-multi-Byte-integer compound-message index))
         (index (+ index 4))
         ;; file-part-length
         (file-part-length (parse-multi-Byte-integer compound-message index))
         (index (+ index 4))
         ;; file-path
         (file-path (substring compound-message index (+ index file-path-length)))
         (index (+ index file-path-length))
         ;; saved-file-path
         (saved-file-path (substring compound-message index (+ index saved-file-path-length)))
         (index (+ index saved-file-path-length))
         ;; file-coding-system
         (file-coding-system (substring compound-message index (+ index file-coding-system-length)))
         (index (+ index file-coding-system-length))
         ;; file-part
         (file-part (substring compound-message index (+ index file-part-length)))
         (new-file-p (zerop (length saved-file-path))))
    ;; choose the directory to save the file and create a random file name
    (when new-file-p
      (let* ((directory (file-name-directory file-path))
             (file-name (file-relative-name file-path directory))
             (file-name-non-ext (file-name-sans-extension file-name))
             (ext (file-name-extension file-name))
             (timestamp-string (number-to-string (time-convert nil 'integer)))
             ;; TODO: decode the file-name
             (file-name (format "%s_%s.%s" file-name-non-ext timestamp-string ext))
             (saving-directory (read-directory-name (format "Choose the directory to save file %s: " file-name))))
        (setq saved-file-path (expand-file-name file-name saving-directory))))
    ;; check the saved file length
    (when (and (file-exists-p saved-file-path)
               (not (= saved-file-length-for-check (file-size saved-file-path))))
      (message "The save file length for check [%d] is not equal to real size [%d], stop saving file."
               saved-file-length-for-check (file-size saved-file-path)))
    ;; write file part
    (p2p-ws-debug-message "Now write part of file into %s, file part length: %d"
                          saved-file-path file-part-length)
    (let* ((file-coding-system (intern file-coding-system))
           (content (decode-coding-string file-part file-coding-system))
           (coding-system-for-write file-coding-system))
      (write-region content nil saved-file-path 'append))
    ;; send back the message
    (let* ((rich-message (format "%s" `((msg-type . file)
                                        (file-path . ,saved-file-path)
                                        (new-file-p . ,new-file-p)
                                        (sender-p . ,(not sender-p))))))
      (p2p-ws-debug-message "Save part of file succeed, length: %S, now notify back..." saved-file-length)
      (p2p-update-websocket-buffer ws rich-message)
      (p2p-do-update-websocket-buffer (websocket-remote-name-with-nickname ws) rich-message t)
      (let* ((message-type 3)
             (sender-p 0)
             (saved-file-path-length (length saved-file-path))
             (back-message (concat (list message-type sender-p)
                                   (integer-to-Byte-list epoch)
                                   (integer-to-Byte-list file-path-length)
                                   (integer-to-Byte-list saved-file-path-length)
                                   file-path
                                   saved-file-path)))
        (p2p-ws-debug-message "notify back message: %s, type: %d, sender-p: %d, epoch: %d" back-message message-type sender-p epoch)
        (websocket-send-text ws back-message)))))

(defun websocket-handle-file-message-to-send (compound-message)
  "Parse COMPOUND-MESSAGE as a file message, send file part."
  (let* ((index 2)
         (epoch (parse-multi-Byte-integer compound-message index))
         (index (+ index 4))
         (file-path-length (parse-multi-Byte-integer compound-message index))
         (index (+ index 4))
         (saved-file-path-length (parse-multi-Byte-integer compound-message index))
         (index (+ index 4))
         (file-path (substring compound-message index (+ index file-path-length)))
         (index (+ index file-path-length))
         (saved-file-path (substring compound-message index (+ index saved-file-path-length))))
    (p2p-ws-debug-message "continue send file part: %s, %s" file-path saved-file-path)
    ;; received the message from file receiver, and then send the next file part
    (let* ((saved-file-length (file-size saved-file-path))
           (begin saved-file-length))
      (p2p-websocket-send-file-part file-path begin saved-file-path saved-file-length (+ epoch 1)))))

(defun websocket-handle-received-message (ws compound-message)
  "Check the COMPOUND-MESSAGE and loop back the certain response to WS."
  (let* ((message-type (aref compound-message 0)))
    (p2p-ws-debug-message "message-type: %d" message-type)
    (cond
     ((eq message-type 1)
      (websocket-handle-text-message compound-message))
     ((eq message-type 2)
      (websocket-handle-buffer-message compound-message))
     ((eq message-type 3)
      (websocket-handle-file-message compound-message))
     (t
      (message "the message type %S does not supported yet" message-type)))))

(defun websocket-server-open-handler (ws)
  "Handle the websocket WS's open event."
  (p2p-ws-debug-message "*** websocket server opened %s ***" (websocket-remote-name-with-nickname ws)))

(defun websocket-server-close-handler (ws)
  "Handle the websocket WS's close event."
  (p2p-ws-debug-message "*** websocket server closed %s ***" (websocket-remote-name-with-nickname ws)))

(defun websocket-server-message-handler (ws frame)
  "Handle the websocket WS's FRAME."
  (when (eq (websocket-frame-opcode frame) 'text)
    (let ((message (websocket-frame-payload frame)))
      (p2p-ws-debug-message ">>> websocket server received message [%s] from %s"
                            (substring message 0 (min 200 (length message)))
                            (websocket-remote-name-with-nickname ws))
      (websocket-handle-received-message ws message))))

;; -*- websocket client -*-

(defun connect-websocket-server (host port)
  "Connect to a websocket server with HOST and PORT."
  ;(interactive "splease input host: \nsplease input port: ")
  (interactive
   (list
    (read-string (format "please input host (default %s): " *p2p-ws-server-host*) nil nil *p2p-ws-server-host*)
    (read-number "please input port: " *p2p-ws-server-port*)))
  (let* ((ws-url (format "ws://%s:%s" host port))
         (ws-list (p2p-ws-filter-with-url ws-url))
         (new-ws))
    (if (> (length ws-list) 0)
        (message "The connection %s already exists!" ws-url)
      (setq new-ws (websocket-open ws-url
                                   :on-message #'websocket-client-message-handler
                                   :on-close #'websocket-client-close-handler))
      (if (websocket-openp new-ws)
          (push new-ws *p2p-ws-client-list*)))
    new-ws))

(defun websocket-client-message-handler (ws frame)
  "Handle the websocket WS's FRAME."
  (when (eq (websocket-frame-opcode frame) 'text)
    (let ((message (websocket-frame-payload frame)))
      (p2p-ws-debug-message ">>> websocket client received message [%s] from: %s"
                            (substring message 0 (min 200 (length message)))
                            (websocket-url ws))
      (websocket-handle-received-message ws message))))

(defun websocket-client-close-handler (ws)
  "Handle the websocket WS's close event."
  (p2p-ws-debug-message "websocket client %s closed" (websocket-url ws))
  (p2p-ws-client-list-remove ws))

(defun websocket-send-message ()
  "Send message though websocket."
  (interactive)
  (p2p-ws-send-text))

(defun p2p-ws-send-text ()
  "Send MESSAGE through the selected websocket connection."
  (interactive)
  (p2p-ws-list-with-action
   (let ((message (completing-read
                   (format "Please input the message to send %s: " (websocket-remote-name-with-nickname ws))
                   nil nil t)))
     (p2p-ws-do-send-text ws message t))))

(defun integer-to-Byte-list (value)
  "Convert VALUE to two Byte list."
  (let* ((ll (logand value #x7F))
         (lh (logand (ash value -7) #x7F))
         (hl (logand (ash value -14) #x7F))
         (hh (logand (ash value -21) #x7F))
         (list (list hh hl lh ll)))
    (p2p-ws-debug-message "value: %S, [%d, %d, %d, %d]" value hh hl lh ll)
    list))

(defun p2p-ws-do-send-text (ws message local-p)
  "Do send the MESSAGE to WS."
  (setq ws (websocket-ensure-connected ws))
  ;; recheck and send the message
  (when (websocket-openp ws)
    (p2p-ws-debug-message "<<< send message [%s] to %s" (substring message 0 (min (length message) 100)) (websocket-remote-name-with-nickname ws))
    (let* ((message-type 1)
           (raw-message (encode-coding-string message 'raw-text))
           (length-list (integer-to-Byte-list (length raw-message)))
           (head (apply #'unibyte-string (cons message-type length-list)))
           (compound-message (concat head raw-message)))
      (p2p-ws-debug-message "compound-message to send: %s" compound-message)
      (websocket-send-text ws compound-message))
    (p2p-do-update-websocket-buffer (websocket-remote-name-with-nickname ws) message local-p)))

(defun websocket-ensure-connected (ws)
  "Reconnect the websocket server of WS."
  (cond
   ;; check the websocket is opened or not
   ((websocket-openp ws)
    ;; the websocket is openp, return the origin websocket
    ws)
   (t
    (message "websocket %s is not opened, open a new again!" (websocket-remote-name-with-nickname ws))
    (websocket-reconnect ws))))

(defun websocket-ensure-server (host port)
  "Reconnect the websocket server of HOST and PORT."
  (when (yes-or-no-p (format "If you're client and %s:%s is server, do you want to reconnect? " host port))
    (connect-websocket-server host port)))

(defun websocket-reconnect (ws)
  "Reconnect the websocket server of WS."
  (cond
   ;; sended websocket
   ((member ws *p2p-ws-client-list*)
    (message "I'm client, now close the connect")
    (websocket-close ws)
    (websocket-do-reconnect ws))
   ;; accepted websocket
   ((member ws websocket-server-websockets)
    (message "I'm server, just close the remote client.")
    (websocket-server-close ws)
    ;; return the origin websocket
    ws)
   (t
    ;; return the origin websocket
    (message "other status")
    (if (yes-or-no-p "If you're websocket client, do you want to reconnect? ")
        (websocket-do-reconnect ws)
      ws))))

(defun websocket-do-reconnect (ws)
  "Just reconnect the websocket server of WS."
  (let* ((conn (websocket-conn ws))
           (conn-info (process-contact conn t))
           (remote-info (mapcar 'identity (plist-get conn-info :remote)))
           (host (format "%s.%s.%s.%s"
                         (first remote-info)
                         (second remote-info)
                         (third remote-info)
                         (fourth remote-info)))
           (port (fifth remote-info)))
      ;; re-send websocket
      (message "I'm client, now reconnect to the server")
      (connect-websocket-server host port)))

(defun websocket-remote-name (ws)
  "Get the WS's sender of remote."
  (if (websocket-p ws)
      (let* ((conn (websocket-conn ws))
             (conn-info (process-contact conn t))
             (remote-info (mapcar 'identity (plist-get conn-info :remote)))
             (sender (format "%s.%s.%s.%s:%s"
                             (first remote-info)
                             (second remote-info)
                             (third remote-info)
                             (fourth remote-info)
                             (fifth remote-info))))
        (p2p-ws-debug-message "remote-name: %s" sender)
        sender)))

(defun websocket-remote-ip (ws)
  "Get the WS's sender of remote."
  (if (websocket-p ws)
      (let* ((conn (websocket-conn ws))
             (conn-info (process-contact conn t))
             (remote-info (mapcar 'identity (plist-get conn-info :remote)))
             (remote-ip (format "%s.%s.%s.%s"
                                (first remote-info)
                                (second remote-info)
                                (third remote-info)
                                (fourth remote-info))))
        (p2p-ws-debug-message "remote-ip: %s" remote-ip)
        remote-ip)))

(defun websocket-local-name (ws)
  "Get the WS's sender of local."
  (if (websocket-p ws)
      (let* ((conn (websocket-conn ws))
             (conn-info (process-contact conn t))
             (local-info (mapcar 'identity (plist-get conn-info :local)))
             (sender (format "%s.%s.%s.%s:%s"
                             (first local-info)
                             (second local-info)
                             (third local-info)
                             (fourth local-info)
                             (fifth local-info))))
        (p2p-ws-debug-message "local-name: %s" sender)
        sender)))

(defun websocket-local-ip (ws)
  "Get the WS's sender of local."
  (if (websocket-p ws)
      (let* ((conn (websocket-conn ws))
             (conn-info (process-contact conn t))
             (local-info (mapcar 'identity (plist-get conn-info :local)))
             (local-ip (format "%s.%s.%s.%s"
                                (first local-info)
                                (second local-info)
                                (third local-info)
                                (fourth local-info))))
        (p2p-ws-debug-message "local-ip: %s" local-ip)
        local-ip)))

(defun websocket-remote-name-with-nickname (ws)
  "Get the websocket remote-name and it's nickname of WS."
  (let ((nickname (p2p-ws-nickname ws)))
    (p2p-ws-debug-message "get nickname: %s" nickname)
    (if (not nickname)
        (websocket-remote-name ws)
      (format "%s (%s)" nickname (websocket-remote-name ws)))))

(defun p2p-ws-nickname (ws)
  "Get the nickname of the websocket WS."
  (alist-get ws *p2p-ws-nickname-list*))

(defun p2p-ws-set-nickname ()
  "."
  (interactive)
  (p2p-ws-list-with-action
   (let ((nickname (completing-read
                   (format "Please input the nickname to send %s: " (websocket-remote-name-with-nickname ws))
                   nil nil t)))
     (p2p-ws-do-set-nickname nickname ws))))

(defun p2p-ws-do-set-nickname (nickname ws)
  "Set NICKNAME for websocket WS."
  (setq ws (websocket-ensure-connected ws))
  ;; recheck and send the message
  (when (websocket-openp ws)
    (p2p-ws-debug-message "set nickname %s to %s" nickname (websocket-remote-name-with-nickname ws))
    (setf (alist-get ws *p2p-ws-nickname-list*) nickname)))

(defmacro p2p-ws-list-with-action (&rest body)
  "List all the server inbound and client outbound websocket, select one and execute the BODY."
  `(let* ((ws-url (completing-read "Select the connection: "
                                   (concatenate
                                    'list
                                    (mapcar #'websocket-remote-name-with-nickname websocket-server-websockets)
                                    (mapcar #'websocket-remote-name-with-nickname *p2p-ws-client-list*))
                                   nil t))
          (ws-list (websocket-inbound-filter-with-url ws-url)))
     (p2p-ws-debug-message "ws-list length: %S" (length ws-list))
     (if (> (length ws-list) 0)
         (let ((ws (car ws-list)))
           (p2p-ws-debug-message "ws opened: %S" (websocket-openp ws))
           ,@body))))

(defun p2p-ws-close ()
  "Send MESSAGE through the selected websocket connection."
  (interactive)
  (p2p-ws-list-with-action
   (when (websocket-openp ws)
     (websocket-close ws))))

(defun p2p-ws-filter-with-url (url)
  "Filter the connections by URL."
  (let (ws-list)
    (dolist (ws *p2p-ws-client-list* ws-list)
      (if (string-equal url (websocket-url ws))
          (push ws ws-list)))))

(defun websocket-inbound-filter-with-url (url)
  "Filter the inbound websocket by URL."
  (let ((origin-ws-list (concatenate 'list websocket-server-websockets *p2p-ws-client-list*))
        ws-list)
    (dolist (ws origin-ws-list ws-list)
      (if (string-equal url (websocket-remote-name-with-nickname ws))
          (push ws ws-list)))))

(defun p2p-ws-client-list-remove (ws)
  "Remove client WS from client list."
  (setq *p2p-ws-client-list* (delete ws *p2p-ws-client-list*)))

(defun p2p-ws-client-list-add (ws)
  "Remove client WS from client list."
  (push ws *p2p-ws-client-list*))

;; -*- p2p-websocket-buffer -*-

(defface p2p-websocket-notice-face
  '((default :weight bold)
    (((class color) (min-colors 88)) :foreground "SlateBlue")
    (t :foreground "blue"))
  "P2P websocket face for notices."
  :group 'p2p-websocket-faces)

(defcustom p2p-websocket-button-mouse-face 'highlight
  "Face used for mouse highlighting in p2p websocket buffers.

Buttons will be displayed in this face when the mouse cursor is
above them."
  :type 'face
  :group 'p2p-websocket-faces)

(defface p2p-websocket-local-face
  '((((class color) (min-colors 88)) :foreground "sea green" :weight bold)
    (t :weight bold))
  "P2P websocket local face."
  :group 'p2p-websokcet-faces)

(defface p2p-websocket-button-face '((t :weight bold))
  "P2P websocket button face."
  :group 'p2p-websokcet-faces)

(defface p2p-websocket-buffer-name-face '((t :weight bold))
  "P2P websocket buffer name face."
  :group 'p2p-websokcet-faces)

(defvar p2p-websocket-aggregate-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map "n" 'p2p-websocket-next-message)
    (define-key map "p" 'p2p-websocket-previous-message)
    (define-key map "r" 'p2p-websocket-reply-this-message)
    (define-key map "R" 'p2p-websocket-reconnect-this-message)
    (define-key map "b" 'p2p-websocket-send-buffer-to-this-message)
    (define-key map "f" 'p2p-websocket-send-file-to-this-message)
    (define-key map "d" 'p2p-websocket-delete-this-message)
    (define-key map "u" 'p2p-websocket-undo)
    (define-key map "q" 'quit-window)
    map))

(defvar p2p-websocket-button-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'p2p-websocket-button-press-button)
    (define-key map (kbd "<mouse-2>") 'p2p-websocket-button-click-button)
    (define-key map [follow-link] 'mouse-face)
    (set-keymap-parent map p2p-websocket-aggregate-mode-map)
    map)
  "Local keymap for PWA buttons.")

(defun p2p-websocket-button-press-button (&rest _ignore)
  "Check text at point for a callback function.
If the text at point has a `p2p-websocket-button-callback' property,
call it with the value of the `pp2-websocket-data' text property."
  (interactive)
  (let* ((data (get-text-property (point) 'p2p-websocket-data))
         (fun (get-text-property (point) 'p2p-websocket-button-callback)))
    (message "data under point is %S" data)
    (message "function under point is %S" fun)
    (unless fun
      (message "No button at point"))
    (when (and fun (symbolp fun) (not (fboundp fun)))
      (error "Function %S is not bound" fun))
    (apply fun data)))

(defun p2p-websocket-button-click-button (_ignore event)
  "Call `p2p-websocket-button-press-button' with EVENT."
  (interactive "P\ne")
  (save-excursion
    (mouse-set-point event)
    (p2p-websocket-button-press-button)))

(defun ensure-p2p-websocket-buffer ()
  "."
  (when (not (buffer-live-p *p2p-websocket-buffer*))
    ;; create buffer if not exists
    (setq *p2p-websocket-buffer*
          (generate-new-buffer "*p2p-websocket-buffer*"))
    ;; set the major mode
    (with-current-buffer *p2p-websocket-buffer*
      (p2p-websocket-aggregate-mode))))

(defun p2p-update-websocket-buffer (ws message)
  "Parse the websocket WS's MESSAGE and update *p2p-websocket-buffer*."
  (let ((sender (websocket-remote-name-with-nickname ws)))
    (p2p-do-update-websocket-buffer sender message nil)))

(defun p2p-do-update-websocket-buffer (sender msg local-p)
  "Update websocket buffer with SENDER, MSG and LOCAL-P flag."
  ;; ensure buffer
  (ensure-p2p-websocket-buffer)
  ;; update buffer
  (with-current-buffer *p2p-websocket-buffer*
    (read-only-mode 0)
    (save-excursion
      (goto-char (point-max))
      (let* ((type "string")
             (data (read msg)))
        (when (consp data)
          (let ((msg-type (alist-get 'msg-type data)))
            (pcase msg-type
              ('buffer
               (setq type "buffer")
               ;; message is buffers.elfer-name of data
               (setq msg (if local-p
                             (format "send buffer: %s" (alist-get 'buffer-name data))
                           (format "received buffer: %s" (alist-get 'buffer-name data)))))
              ('file
               (setq type "file")
               ;; message is buffers.elfer-name of data
               (let ((new-file-p (alist-get 'new-file-p data))
                     (sender-p (alist-get 'sender-p data)))
                 (cond
                  (new-file-p
                   (let* ((file-path (format "%s" (alist-get 'file-path data)))
                          (directory (file-name-directory file-path))
                          (file-name (file-relative-name file-path directory)))
                     (setq msg (if sender-p
                                   (format "send file: %s" file-name)
                                 (format "received file: %s" file-name)))))
                  (t
                   (setq msg nil)))))
              (_
               ;; message is content of data
               (setq msg (alist-get 'content data))))))
        ;; insert content
        (when (> (length msg) 0)
          (let* ((now (format-time-string "%Y-%m-%d %a %H:%M:%S" (current-time)))
                 (content (format "%s [%s]:\n%s\n\n" sender now msg))
                 (new-msg-start (point-max))
                 (sender-start new-msg-start)
                 (sender-end (+ sender-start (length sender)))
                 (time-start (- (s-index-of now content) 1))
                 (time-end (+ time-start (length now) 2))
                 (msg-start (+ new-msg-start time-end 2))
                 (msg-end (+ msg-start (length msg)))
                 (sender-face (if local-p 'p2p-websocket-local-face 'p2p-websocket-button-face)))
            ;; add the sender property
            (put-text-property 0 (length sender)
                               'font-lock-face sender-face content)
            ;; add the time property
            (put-text-property time-start time-end
                               'font-lock-face 'p2p-websocket-notice-face content)
            ;; insert the whole message
            (insert content)
            ;; add sender button
            (p2p-websocket-add-target-button sender-start sender-end sender)
            ;; add content button
            (cond
             ((equal type "buffer")
              (p2p-websocket-add-buffer-button msg-start msg-end (format "%s" (alist-get 'buffer-name data))))
             ((equal type "file")
              (let* ((sender-p (alist-get 'sender-p data))
                     (file-path (alist-get 'file-path data)))
                (p2p-websocket-add-file-button msg-start msg-end (format "%s" file-path)))))))
        ))
    (read-only-mode t))
  ;; display buffer
  (display-buffer *p2p-websocket-buffer*))

(defun p2p-websocket-add-target-button (start end sender)
  "Add a button property to from START to END with target SENDER."
  (add-text-properties
   start end
   (nconc
    (list 'keymap p2p-websocket-button-keymap)
    (list 'p2p-websocket-button-callback #'p2p-websocket-sender-callback)
    (list 'p2p-websocket-data (list sender))
    (list 'mouse-face p2p-websocket-button-mouse-face)
    (list 'rear-nonsticky t))))

(defun p2p-websocket-add-buffer-button (start end buffer-name)
  "Add a buffer property to from START to END with BUFFER-NAME."
  (add-text-properties
   start end
   (nconc
    (list 'keymap p2p-websocket-button-keymap)
    (list 'p2p-websocket-button-callback #'p2p-websocket-buffer-callback)
    (list 'p2p-websocket-data (list buffer-name))
    (list 'mouse-face p2p-websocket-button-mouse-face)
    (list 'rear-nonsticky t))))

(defun p2p-websocket-add-file-button (start end file-path)
  "Add a file property to from START to END with FILE-PATH."
  (add-text-properties
   start end
   (nconc
    (list 'keymap p2p-websocket-button-keymap)
    (list 'p2p-websocket-button-callback #'p2p-websocket-file-callback)
    (list 'p2p-websocket-data (list file-path))
    (list 'mouse-face p2p-websocket-button-mouse-face)
    (list 'rear-nonsticky t))))

(defun p2p-websocket-sender-callback (data)
  "Callback of the p2p websocket sender button with DATA as args."
  (let ((ws (p2p-websocket-parse-from-data data))
        (message (read-from-minibuffer "Please input the message to send: ")))
    (unless ws
      (let* ((array (split-string data ":"))
             (host (car array))
             (port (cadr array)))
        (setq ws (websocket-ensure-server host port))))
    (p2p-ws-do-send-text ws message t)))

(defun p2p-websocket-buffer-callback (data)
  "Callback of the p2p websocket buffer button with DATA as args."
  (let* ((buffer-name data)
         (buffer (get-buffer buffer-name)))
    (when buffer
      (switch-to-buffer buffer))))

(defun p2p-websocket-file-callback (data)
  "Callback of the p2p websocket file button with DATA as args."
  (let ((file-path data))
    (dired-jump t file-path)))

(defun p2p-websocket-parse-from-data (data)
  "Callback of the p2p websocket sender button with DATA as args."
  (when (and data (> (length data) 0))
    (let* ((remote-name data)
           (ws-list (websocket-inbound-filter-with-url remote-name)))
      (when (> (length ws-list) 0)
          (car ws-list)))))

(defun p2p-websocket-header-p ()
  "."
  (eq 'p2p-websocket-sender-callback
      (get-text-property (point) 'p2p-websocket-button-callback)))

(defun p2p-websocket-current-message ()
  "Goto the current message's head in *p2p-websocket-buffer*."
  (interactive)
  (beginning-of-visual-line)
  (loop while (and (not (p2p-websocket-header-p))
                   (not (eq (point) (point-min)))
                   (not (eq (point) (point-max))))
        do (previous-line)))

(defun p2p-websocket-next-message ()
  "Goto the next message's head in *p2p-websocket-buffer*."
  (interactive)
  (beginning-of-visual-line)
  (forward-line)
  (loop while (and (not (p2p-websocket-header-p))
                   (not (eq (point) (point-min)))
                   (not (eq (point) (point-max))))
        do (forward-line)))

(defun p2p-websocket-previous-message ()
  "Goto the previous message's head in *p2p-websocket-buffer*."
  (interactive)
  (beginning-of-visual-line)
  (previous-line)
  (loop while (and (not (p2p-websocket-header-p))
                   (not (eq (point) (point-min)))
                   (not (eq (point) (point-max))))
        do (previous-line)))

(defun p2p-websocket-reply-this-message ()
  "Reply this current message."
  (interactive)
  (save-excursion
    (p2p-websocket-current-message)
    (p2p-websocket-button-press-button)))

(defun p2p-websocket-reconnect-this-message ()
  "Reconnect the current message's ws."
  (interactive)
  (save-excursion
    (p2p-websocket-current-message)
    (let* ((data (get-text-property (point) 'p2p-websocket-data))
           (ws (p2p-websocket-parse-from-data (car data))))
      (websocket-reconnect ws))))

(defun p2p-websocket-send-buffer-to-this-message ()
  "Reply this current message with buffer."
  (interactive)
  (save-excursion
    (p2p-websocket-current-message)
    (let* ((data (get-text-property (point) 'p2p-websocket-data))
           (ws (p2p-websocket-parse-from-data (car data)))
           (buffer (completing-read "Choose the buffer: " #'internal-complete-buffer nil t))
           (buffer-content (with-current-buffer buffer (buffer-substring-no-properties (point-min) (point-max)))))
      (let* ((message-type 2)
             (buffer-name (encode-coding-string buffer 'raw-text))
             (buffer-name-len-list (integer-to-Byte-list (length buffer-name)))
             (raw-message (encode-coding-string buffer-content 'raw-text))
             (message-len-list (integer-to-Byte-list (length raw-message)))
             (compound-message (concat (list message-type)
                                       buffer-name-len-list buffer-name
                                       message-len-list raw-message)))
        (p2p-ws-debug-message "compound-message to send: %s" compound-message)
        (websocket-send-text ws compound-message)
        ;; update buffer
        (let ((rich-message (format "%s" `((msg-type . buffer) (buffer-name . ,buffer-name)))))
          (p2p-do-update-websocket-buffer (websocket-remote-name-with-nickname ws) rich-message t))
        ))))

(defun p2p-websocket-send-file-to-this-message ()
  "Reply this current message with buffer."
  (interactive)
  (save-excursion
    (p2p-websocket-current-message)
    (let* ((data (get-text-property (point) 'p2p-websocket-data))
           (ws (p2p-websocket-parse-from-data (car data)))
           (file-path (completing-read "Choose the file: " #'read-file-name-internal nil t)))
      (p2p-websocket-send-file-part file-path 0 "" 0 0))))

(defun file-size (file-path)
  "Get the file size of FILE-PATH."
  (when-let ((command (pcase window-system
                        ('ns "/usr/local/bin/gstat")
                        ('x "/usr/bin/stat")
                        (_ nil))))
    (with-temp-buffer
      (when (= 0 (call-process command nil t nil "-c %s" file-path))
        (string-to-number (s-trim (buffer-string)))))))

(defun p2p-websocket-send-file-part (file-path begin saved-file-path saved-file-length-for-check epoch)
  "Send part of file, parameter is FILE-PATH, BEGIN, SAVED-FILE-PATH, SAVED-FILE-LENGTH-FOR-CHECK, EPOCH."
  (let* ((file-size (file-size file-path))
         (end (min file-size (+ begin 4096))))
    (cond
     ((> end begin)
      (p2p-ws-debug-message "file-size: %S, saved-file-length-for-check: %S, begin: %S, end: %S"
                            file-size saved-file-length-for-check begin end)
      ;; message-type, sender-p, epoch, file-path, saved-file-path, file-coding-system, file-part
      (let* ((message-type 3)
             (sender-p 1)
             (file-coding-system)
             (file-part (with-temp-buffer
                          (insert-file-contents file-path nil begin end)
                          (setq file-coding-system buffer-file-coding-system)
                          (buffer-string)))
             (file-part (string-as-unibyte file-part))
             (file-part-length (length file-part))
             (file-coding-system (symbol-name file-coding-system))
             (file-coding-system (string-as-unibyte file-coding-system))
             (file-coding-system-length (length file-coding-system))
             ;; unibyte
             (file-path (string-as-unibyte file-path))
             (file-path-length (length file-path))
             (saved-file-path (string-as-unibyte saved-file-path))
             (saved-file-path-length (length saved-file-path))
             (compound-message (concat (list message-type sender-p)
                                       (integer-to-Byte-list epoch)
                                       (integer-to-Byte-list saved-file-length-for-check)
                                       (integer-to-Byte-list file-path-length)
                                       (integer-to-Byte-list saved-file-path-length)
                                       (integer-to-Byte-list file-coding-system-length)
                                       (integer-to-Byte-list file-part-length)
                                       file-path
                                       saved-file-path
                                       file-coding-system
                                       file-part)))
        (p2p-ws-debug-message "file part compound-message to send: %s, length: %d"
                              (substring compound-message 0 (min 200 (length compound-message)))
                              (length compound-message))
        (p2p-ws-debug-message "send...\n%d %d\n%d\n%d\n%d\n%d\n%d\n%d\n%s\n%s\n%s"
                              message-type sender-p
                              epoch
                              saved-file-length-for-check
                              file-path-length
                              saved-file-path-length
                              file-coding-system-length
                              file-part-length
                              file-path
                              saved-file-path
                              file-coding-system)
        (when (= begin 0)
          (let* ((rich-message (format "%s" `((msg-type . file)
                                              (file-path . ,file-path)
                                              (new-file-p . t)
                                              (sender-p . ,sender-p)))))
            (p2p-do-update-websocket-buffer (websocket-remote-name-with-nickname ws) rich-message t)))
        (websocket-send-text ws compound-message)))
     ((= end begin)
      (message "Sending file succeed!"))
     (t
      (message "Send file part: wrong file parameter begin and end.")))))

(defun p2p-websocket-delete-this-message ()
  "Delete the message in *p2p-websocket--buffer*."
  (interactive)
  (read-only-mode 0)
  (p2p-websocket-current-message)
  (let ((msg-start (point)))
    (p2p-websocket-next-message)
    (let ((msg-end (point)))
      (delete-region msg-start msg-end)))
  (read-only-mode 1))

(defun p2p-websocket-undo ()
  "Undo in the *p2p-websocket-buffer*."
  (interactive)
  (read-only-mode 0)
  (undo)
  (read-only-mode 1))

(define-derived-mode p2p-websocket-aggregate-mode text-mode "PWA"
  ;; The mode for *p2p-websocket-buffer*.
  (use-local-map p2p-websocket-aggregate-mode-map))

(provide 'p2p-websocket)
;;; p2p-websocket.el ends here
