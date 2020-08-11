;;; p2p-websocket.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2020 gadmyth

;; Author: p2p-websocket.el <gadmyth@gmail.com}>
;; Version: 0.1.4
;; Package-Version: 20200808.001
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

(defvar *p2p-ws-server* nil)
(defvar *p2p-ws-server-host* "0.0.0.0")
(defvar *p2p-ws-server-port* 3000)
(defvar *p2p-ws-client-list* '())

(defvar *p2p-websocket-buffer* nil)

;; -*- websocket server -*-

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
  (message "*** websocket server opened %s ***" (websocket-remote-name ws)))

(defun websocket-server-close-handler (ws)
  "Handle the websocket WS's close event."
  (message "*** websocket server closed %s ***" (websocket-remote-name ws)))

(defun websocket-server-message-handler (ws frame)
  "Handle the websocket WS's FRAME."
  (let ((message (websocket-frame-text frame)))
    (message "*** websocket server received message from %s ***" (websocket-remote-name ws))
    (p2p-update-websocket-buffer ws frame)
    (cond ((equal "hello" message)
           (websocket-send-text ws "world"))
          ((equal "ping" message)
           (websocket-send-text ws "pong")))))

;; -*- websocket client -*-

(defun connect-websocket-server (host port)
  "Connect to a websocket server with HOST and PORT."
  (interactive "splease input host: \nsplease input port: ")
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
    (message "websocket client received message from: %s" (websocket-url ws))
    (p2p-update-websocket-buffer ws frame)))

(defun websocket-client-close-handler (ws)
  "Handle the websocket WS's close event."
  (message "websocket client %s closed" (websocket-url ws))
  (p2p-ws-client-list-remove ws))

(defun websocket-send-message (message)
  "Send MESSAGE though websocket."
  (interactive "splease input the message to send: ")
  (p2p-ws-send-text message))

(defun p2p-ws-send-text (message)
  "Send MESSAGE through the selected websocket connection."
  (interactive "splease input the message to send: ")
  (p2p-ws-list-with-action
   (when (not (websocket-openp ws))
     (message "websocket %s is not opened, open a new again!" (websocket-remote-name nil))
     (p2p-ws-client-list-remove ws)
     (setq ws (websocket-ensure-connected ws)))
   (when (websocket-openp ws)
     (message "send message to %s" (websocket-remote-name ws))
     (websocket-send-text ws message))))

(defmacro p2p-ws-list-with-action (&rest body)
  "List all the server inbound and client outbound websocket, select one and execute the BODY."
  `(let* ((ws-url (completing-read "Select the connection: "
                                   (concatenate
                                    'list
                                    (mapcar #'websocket-remote-name websocket-server-websockets)
                                    (mapcar #'websocket-remote-name *p2p-ws-client-list*))
                                   nil t))
          (ws-list (websocket-inbound-filter-with-url ws-url)))
     (message "ws-list length: %S" (length ws-list))
     (if (> (length ws-list) 0)
         (let ((ws (car ws-list)))
           (message "ws opened: %S" (websocket-openp ws))
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
      (if (string-equal url (websocket-remote-name ws))
          (push ws ws-list)))))

(defun p2p-ws-client-list-remove (ws)
  "Remove client WS from client list."
  (setq *p2p-ws-client-list* (delete ws *p2p-ws-client-list*)))

(defun p2p-ws-client-list-add (ws)
  "Remove client WS from client list."
  (push ws *p2p-ws-client-list*))

;; -*- p2p-websocket-buffer -*-

(defun p2p-update-websocket-buffer (ws frame)
  "Parse the websocket WS's FRAME message and update *p2p-websocket-buffer*."
  (when (not (buffer-live-p *p2p-websocket-buffer*))
    ;; create buffer if not exists
    (setq *p2p-websocket-buffer*
          (generate-new-buffer "*p2p-websocket-buffer*"))
    ;; set the major mode
    (with-current-buffer *p2p-websocket-buffer*
      (p2p-websocket-aggregate-mode)))
  
  (with-current-buffer *p2p-websocket-buffer*
    (erc-save-excursion
     (goto-char (point-max))
     (let* ((now (format-time-string "%Y-%m-%d %a %H:%M:%S" (current-time)))
            (sender (websocket-remote-name ws))
            (msg (websocket-frame-text frame))
            (content (format "%s [%s]:\n%s\n\n" sender now msg))
            (new-msg-start (point-max))
            (time-start (- (s-index-of now content) 1))
            (time-end (+ time-start (length now) 2))
            (msg-start (+ time-end 2))
            (msg-end (+ msg-start (length msg))))
     (erc-put-text-property time-start time-end
                            'font-lock-face 'erc-notice-face content)
      (insert content)
      (display-buffer *p2p-websocket-buffer*)))))

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
        (message "remote-name: %s" sender)
        sender)))

(defvar p2p-websocket-aggregate-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map "q" 'quit-window)
    map))

(define-derived-mode p2p-websocket-aggregate-mode text-mode "PWA"
  ;; The mode for *p2p-websocket-buffer*.
  (use-local-map erc-aggregate-mode-map))

(provide 'p2p-websocket)
;;; p2p-websocket.el ends here