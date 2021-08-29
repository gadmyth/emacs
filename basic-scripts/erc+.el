;;; erc+.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2021 gadmyth

;; Author: erc+.el <gadmyth@gmail.com>
;; Version: 1.0.008
;; Package-Version: 20210829.002
;; Package-Requires: erc, s, text-mode, system-util
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


(defvar erc-nick nil)
(defvar *erc-debug* nil)
(defvar *erc-aggregate-buffer* nil)
(defvar erc-aggregate-refresh t)
(defvar erc-aggregate-auto-display nil)
(defvar erc-default-width 100)
(defvar *erc-forbidden-targets* nil)

(defun erc-toggle-debug ()
  "."
  (interactive)
  (setq *erc-debug* (not *erc-debug*))
  (message "turn %s the *erc-debug*" (if *erc-debug* "on" "off")))

(defmacro erc-message (format-string &rest ARGS)
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
    (erc-message "erc message: %s, %s: %s" short-sender tgt msg)
    (when (not (member tgt *erc-forbidden-targets*))
      (erc-update-aggregate-buffer short-sender tgt msg))
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
          (erc-message "no save excursion")
          ,@body)
        (t
         (save-excursion
           (erc-message "save excursion")
           ,@body))))

(defun* erc-update-aggregate-buffer (short-sender target msg)
  "Append SENDER and MSG to the *erc-aggregate-buffer*."
  (let ((sender (erc-sender-with-group short-sender target))
        erc-msg-link erc-link-start erc-link-end
        msg-file-path msg-picture-p)
    (if-let ((wechat-msg (erc-parse-wechat-share-msg sender msg)))
        (setq msg wechat-msg)
      (return-from erc-update-aggregate-buffer))

    ;; parse the wechat file msg as picture, file or emoji
    (when-let ((file-msg (erc-parse-wechat-file-msg msg)))
      (setq msg (car file-msg))
      (setq msg-file-path (cdr file-msg))
      (when (or (string-equal "图片" (car file-msg))
                (string-equal "表情" (car file-msg)))
        (setq msg "")
        (setq msg-picture-p t)))
  
    (when (not (buffer-live-p *erc-aggregate-buffer*))
      ;; create buffer if not exists
      (setq *erc-aggregate-buffer*
            (generate-new-buffer "*erc-aggregate-buffer*"))
      ;; set the major mode
      (with-current-buffer *erc-aggregate-buffer*
        (erc-aggregate-mode)))

    ;; parse the link
    (when-let ((index (string-match "\\(https?://[^\s]*\\)" msg)))
      (setq erc-link-start index)
      (setq erc-msg-link (substring msg (match-beginning 1) (match-end 1)))
      (setq msg (concat (substring msg 0 (match-beginning 1)) "链接" (substring msg (match-end 1))))
      (setq erc-link-end (+ erc-link-start 2)))
    
    ;; append msg
    (with-current-buffer *erc-aggregate-buffer*
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
         (insert "\n"))))
    ;; show window
    (when erc-aggregate-auto-display
      (display-buffer *erc-aggregate-buffer*))))

(defun erc-button-nick-callback (data)
  "When click the nick name erc-button, response with the DATA."
  (let* ((short-sender (car data))
         (target (cdr data))
         (channel (and (string-prefix-p "#" target) target))
         (buffer-name (or channel
                          (and (string-equal target erc-nick) short-sender)
                          target)))
    (erc-message "nick click, data: %s, short-sender: %s, channel: %s, buffer-name: %s" data short-sender channel buffer-name)
    (let* ((buffer (get-buffer buffer-name))
           (window (get-buffer-window buffer)))
      (display-buffer buffer)
      (if (and window (window-live-p window))
          (select-window window)))))

(defun erc-button-file-callback (data)
  "When click the nick name erc-button, response with the DATA."
  ;; file(open-file), picture(show)
  (message "file path: %s" data)
  (if (not (fboundp 'open-file-by-system))
      (require 'system-util))
  (apply #'open-file-by-system (list data)))

(defvar *erc-browse-function-list* '((browse-url-default-browser . browse-url)
                                     (eww-browse-url . browse-url)
                                     (w3m-browse-url . browse-url)
                                     (browse-url-firefox . browse-url)
                                     (browse-url-chrome . browse-url)
                                     (erc-show-link-url . erc-show-link-url)))

(defvar *erc-image-action-list* '(erc-ffap-image erc-open-image erc-image-path))

(defun erc-show-link-url (url)
  "Show erc link's URL."
  (message "erc link url is: %s" url))

(defun erc-button-link-callback (data)
  "When click the nick name erc-button, response with the DATA to open the link."
  (let* ((action-list *erc-browse-function-list*)
         (action (intern (completing-read "Select the browser: "
                                          action-list nil t)))
         (func (alist-get action action-list nil nil #'string=))
          (browse-url-browser-function action))
    (erc-message "browser function is: %s" browse-url-browser-function)
    (if (equal func #'browse-url)
        (if (not (fboundp browse-url-browser-function))
            (message "%s is not implemented!" browse-url-browser-function)
          (browse-url data))
      (funcall func data))))

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

(defvar erc-aggregate-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map "q" 'quit-window)
    (define-key map (kbd "<mouse-3>") 'erc-right-click)
    map))

(define-derived-mode erc-aggregate-mode text-mode "ErcA"
  ;; The mode for *erc-aggregate-buffer*.
  (use-local-map erc-aggregate-mode-map))

(provide 'erc+)
;;; erc+.el ends here
