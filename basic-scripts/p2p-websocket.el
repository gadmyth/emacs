;;; p2p-websocket.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2020 gadmyth

;; Author: p2p-websocket.el <gadmyth@gmail.com}>
;; Version: 0.2.1
;; Package-Version: 20211115.001
;; Package-Requires: websocket
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

(defun websocket-server-open-handler (ws)
  "Handle the websocket WS's open event."
  (p2p-ws-debug-message "*** websocket server opened %s ***" (websocket-remote-name-with-nickname ws)))

(defun websocket-server-close-handler (ws)
  "Handle the websocket WS's close event."
  (p2p-ws-debug-message "*** websocket server closed %s ***" (websocket-remote-name-with-nickname ws)))

(defun websocket-server-message-handler (ws frame)
  "Handle the websocket WS's FRAME."
  (let ((message (websocket-frame-text frame)))
    (p2p-ws-debug-message "*** websocket server received message from %s ***" (websocket-remote-name-with-nickname ws))
    (p2p-update-websocket-buffer ws frame)
    (cond ((equal "hello" message)
           (p2p-ws-do-send-text ws "world" t))
          ((equal "ping" message)
           (p2p-ws-do-send-text ws "pong" t)))))

;; -*- websocket client -*-

(defun connect-websocket-server (host port)
  "Connect to a websocket server with HOST and PORT."
  ;(interactive "splease input host: \nsplease input port: ")
  (interactive
   (list
    (read-string (format "please input host (default %s): " *p2p-ws-server-host*) nil nil *p2p-ws-server-host*)
    (read-number "please input port: " *p2p-ws-server-port*)))
  (let* ((ws-url (format "ws://%s:%s" host port))
         (ws-list (p2p-ws-filter-with-url ws-url)))
    (if (> (length ws-list) 0)
        (message "The connection %s already exists!" ws-url)
      (let ((new-ws (websocket-open ws-url
                                    :on-message #'websocket-client-message-handler
                                    :on-close #'websocket-client-close-handler)))
        (if (websocket-openp new-ws)
            (push new-ws *p2p-ws-client-list*))))))

(defun websocket-client-message-handler (ws frame)
  "Handle the websocket WS's FRAME."
  (let ((message (websocket-frame-text frame)))
    (p2p-ws-debug-message "websocket client received message from: %s" (websocket-url ws))
    (p2p-update-websocket-buffer ws frame)))

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

(defun p2p-ws-do-send-text (ws message local-p)
  "Do send the MESSAGE to WS."
  ;; check the websocket is opened or not
  (when (not (websocket-openp ws))
    (message "websocket %s is not opened, open a new again!" (websocket-remote-name-with-nickname nil))
    (p2p-ws-client-list-remove ws)
    (setq ws (websocket-ensure-connected ws)))
  ;; recheck and send the message
  (when (websocket-openp ws)
    (p2p-ws-debug-message "send message to %s" (websocket-remote-name-with-nickname ws))
    (websocket-send-text ws message)
    (p2p-do-update-websocket-buffer (websocket-local-name ws) message local-p)))

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
  (when (not (websocket-openp ws))
    (message "websocket %s is not opened, open a new again!" (websocket-remote-name-with-nickname ws))
    (p2p-ws-client-list-remove ws)
    (setq ws (websocket-ensure-connected ws)))
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
  "p2p websocket face for notices."
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
  "p2p websocket local face."
  :group 'p2p-websokcet-faces)

(defface p2p-websocket-button-face '((t :weight bold))
  "p2p websocket button face."
  :group 'p2p-websokcet-faces)

(defvar p2p-websocket-aggregate-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map "n" 'p2p-websocket-next-message)
    (define-key map "p" 'p2p-websocket-previous-message)
    (define-key map "r" 'p2p-websocket-reply-this-message)
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
    (unless fun
      (message "No button at point"))
    (when (and fun (symbolp fun) (not (fboundp fun)))
      (error "Function %S is not bound" fun))
    (apply fun data)))

(defun p2p-websocket-button-click-button (_ignore event)
  "Call `p2p-websocket-button-press-button'."
  (interactive "P\ne")
  (save-excursion
    (mouse-set-point event)
    (p2p-websocket-button-press-button)))

(defun ensure-p2p-websocket-buffer ()
  (when (not (buffer-live-p *p2p-websocket-buffer*))
    ;; create buffer if not exists
    (setq *p2p-websocket-buffer*
          (generate-new-buffer "*p2p-websocket-buffer*"))
    ;; set the major mode
    (with-current-buffer *p2p-websocket-buffer*
      (p2p-websocket-aggregate-mode))))

(defun p2p-update-websocket-buffer (ws frame)
  "Parse the websocket WS's FRAME message and update *p2p-websocket-buffer*."
  (let ((sender (websocket-remote-name-with-nickname ws))
        (msg (websocket-frame-text frame)))
    (p2p-do-update-websocket-buffer sender msg nil)))

(defun p2p-do-update-websocket-buffer (sender msg local-p)
  ;; ensure buffer
  (ensure-p2p-websocket-buffer)
  ;; update buffer
  (with-current-buffer *p2p-websocket-buffer*
    (read-only-mode 0)
    (save-excursion
      (goto-char (point-max))
      (let* ((now (format-time-string "%Y-%m-%d %a %H:%M:%S" (current-time)))
             (content (format "%s [%s]:\n%s\n\n" sender now msg))
             (new-msg-start (point-max))
             (sender-start new-msg-start)
             (sender-end (+ sender-start (length sender)))
             (time-start (- (s-index-of now content) 1))
             (time-end (+ time-start (length now) 2))
             (msg-start (+ time-end 2))
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
        ))
    (read-only-mode t))
  
  (display-buffer *p2p-websocket-buffer*))

(defun p2p-websocket-add-target-button (start end sender)
  "Add a button property to from START to END with target SENDER."
  (add-text-properties
   start end
   (nconc (list 'mouse-face p2p-websocket-button-mouse-face)
          (list 'p2p-websocket-button-callback #'p2p-websocket-sender-callback)
          (list 'keymap p2p-websocket-button-keymap)
          (list 'rear-nonsticky t)
          (list 'p2p-websocket-data (list sender)))))

(defun p2p-websocket-sender-callback (data)
  "Callback of the p2p websocket sender button with DATA as args."
  (when (and data (> (length data) 0))
    (let* ((remote-name data)
           (ws-list (websocket-inbound-filter-with-url remote-name)))
      (if (> (length ws-list) 0)
          (let ((ws (car ws-list))
                (message (read-from-minibuffer "Please input the message to send: ")))
            (p2p-ws-do-send-text ws message t))))))

(defun p2p-websocket-header-p ()
  "."
  (eq 'p2p-websocket-sender-callback
      (get-text-property (point) 'p2p-websocket-button-callback)))

(defun p2p-websocket-current-message ()
  "Goto the current message's head in *p2p-websocket-buffer*."
  (interactive)
  (loop while (and (not (p2p-websocket-header-p))
                   (not (eq (point) (point-min)))
                   (not (eq (point) (point-max))))
        do (previous-line)))

(defun p2p-websocket-next-message ()
  "Goto the next message's head in *p2p-websocket-buffer*."
  (interactive)
  (forward-line)
  (loop while (and (not (p2p-websocket-header-p))
                   (not (eq (point) (point-min)))
                   (not (eq (point) (point-max))))
        do (forward-line)))

(defun p2p-websocket-previous-message ()
  "Goto the previous message's head in *p2p-websocket-buffer*."
  (interactive)
  (previous-line)
  (loop while (and (not (p2p-websocket-header-p))
                   (not (eq (point) (point-min)))
                   (not (eq (point) (point-max))))
        do (previous-line)))

(defun p2p-websocket-reply-this-message ()
  "."
    "Reply this current message."
  (interactive)
  (save-excursion
    (p2p-websocket-current-message)
    (p2p-websocket-button-press-button)))

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
