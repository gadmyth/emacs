;;; p2p-websocket.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2020 gadmyth

;; Author: p2p-websocket.el <gadmyth@gmail.com}>
;; Version: 0.1.2
;; Package-Version: 20200806.001
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
(defvar *p2p-ws-client-conns* '())


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
  (message "server websocket opened"))

(defun websocket-server-close-handler (ws)
  "Handle the websocket WS's close event."
  (message "server websocket closed"))

(defun websocket-server-message-handler (ws frame)
  "Handle the websocket WS's FRAME."
  (let ((message (websocket-frame-text frame)))
    (message "server received websocket message: %s" message)
    (if (equal "hello" message)
        (send-websocket-message ws "world"))))

(defun send-websocket-message (ws message)
  "Send MESSAGE though websocket WS."
  (websocket-send-text ws message))

;; -*- websocket client -*-

(defun connect-websocket-server (host port)
  "Connect to a websocket server with HOST and PORT."
  (interactive "splease input host: \nsplease input port: ")
  (let* ((ws-url (format "ws://%s:%s" host port))
         (ws-conns (p2p-ws-filter-connection ws-url)))
    (message "%S" ws-conns)
    (if (> (length ws-conns) 0)
        (message "The connection %s already exists!" ws-url)
      (let ((new-ws-conn (websocket-open ws-url
                                         :on-message #'websocket-client-message-handler
                                         :on-close #'websocket-client-close-handler)))
        (push new-ws-conn *p2p-ws-client-conns*)))))

(defun websocket-client-message-handler (ws frame)
  "Handle the websocket WS's FRAME."
  (let ((message (websocket-frame-text frame)))
    (message "client received websocket message: %s" message)))

(defun websocket-client-close-handler (ws)
  "Handle the websocket WS's close event."
  (message "client websocket %s closed" (websocket-url ws)))

(defun websocket-send-message (message)
  "Send MESSAGE though websocket."
  (interactive "splease input the message to send: ")
  (p2p-ws-send-text message))

(defun p2p-ws-send-text (message)
  "Send MESSAGE through the selected websocket connection."
  (interactive "splease input the message to send: ")
  (let* ((ws-url (completing-read "Select the connection: "
                                  (mapcar #'websocket-url *p2p-ws-client-conns*) nil t))
         (ws-conns (p2p-ws-filter-connection ws-url)))
    (if (> (length ws-conns) 0)
        (let ((conn (car ws-conns)))
          (send-websocket-message conn message)))))

(defun p2p-ws-filter-connection (url)
  "Filter the connections by URL."
  (let (ws-conns)
    (dolist (conn *p2p-ws-client-conns* ws-conns)
      (if (string-equal url (websocket-url conn))
          (push conn ws-conns)))))

(provide 'p2p-websocket)
;;; p2p-websocket.el ends here
