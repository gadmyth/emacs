;;; p2p-websocket.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2020 gadmyth

;; Author: p2p-websocket.el <gadmyth@gmail.com}>
;; Version: 0.1.0
;; Package-Version: 20200805.001
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
​
;; -*- websocket client -*-

(defun connect-websocket-server (host port)
  "Connect to a websocket server with HOST and PORT."
  (interactive "splease input host: \nsplease input port: ")
  (setq my-websocket
        (websocket-open (format "ws://%s:%s" host port)
                      :on-message #'websocket-client-message-handler
                      :on-close #'websocket-client-close-handler)))

(defun websocket-client-message-handler (ws frame)
  "Handle the websocket WS's FRAME."
  (let ((message (websocket-frame-text frame)))
    (message "client received websocket message: %s" message)))

(defun websocket-client-close-handler (ws)
  "Handle the websocket WS's close event."
  (message "client websocket closed"))

(defun send-websocket-message-1 (message)
  "Send MESSAGE though websocket."
  (interactive "splease input the message to send: ")
  (websocket-send-text my-websocket message))

(provide 'p2p-websocket)
;;; p2p-websocket.el ends here
