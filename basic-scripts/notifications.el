;;; notifications.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2021 gadmyth

;; Author: notifications.el <gadmyth@gmail.com>
;; Version: 1.0.6
;; Package-Version: 20211205.002
;; Package-Requires: async, dates, codec
;; Keywords: notification, notify
;; Homepage: https://www.github.com/gadmyth/emacs
;; URL: https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/notifications.el

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
;; notifications's code can be found here:
;;   https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/notifications.el

;;; Commentary:
;;; Code:


(require 'async)
(require 'dates)
(require 'codec)

(defvar *notifications* nil)

(defvar *notifications-buffer* "*notification*")

(defvar +notifications-file-name+ (expand-file-name "~/.emacs.notifications"))

(defun list-notifications ()
  "."
  (interactive)
  (let* ((notifications (mapcar (lambda (notification)
                                  (let* ((message (cdr (assq 'message notification)))
                                         (message (base64-decode-string-as-multibyte message))
                                         (timestamp (cdr (assq 'timestamp notification)))
                                         (timestr (timestamp-to-normal-string timestamp)))
                                    (format "%s (%s): %s" timestr (notification-time-diff-description timestamp) message)))
                                *notifications*))
         (notifications (sort notifications #'string<))
         (current-timestr (current-time-normal-string)))
    (completing-read (format "The notifications (%s): " current-timestr) notifications)))

(defun notification-time-diff-description (timestamp)
  "Show the time diff description of TIMESTAMP and now."
  (let* ((now (current-timestamp))
         (diff (- timestamp now))
         (seconds diff)
         (minutes (/ diff 60))
         (hours (/ diff 3600))
         (days (/ diff 86400))
         (months (/ diff (* 86400 30)))
         (years (/ diff (* 86400 365)))
         (desc ""))
    (when (>= years 1)
      (setq desc (s-concat desc (format "%dy" years))))
    (when (>= months 1)
      (when (> months 12)
        (setq months (mod months 12)))
      (setq desc (s-concat desc (format "%dm" months))))
    (when (>= days 1)
      (when (> days 30)
        (setq days (mod days 30)))
      (setq desc (s-concat desc (format "%dd" days))))
    (when (>= hours 1)
      (when (> hours 24)
        (setq hours (mod hours 24)))
      (setq desc (s-concat desc (format "%dh" hours))))
    (when (>= minutes 1)
      (when (> minutes 60)
        (setq minutes (mod minutes 60)))
      (setq desc (s-concat desc (format "%dm" minutes))))
    (when (>= seconds 1)
      (when (> seconds 60)
        (setq seconds (mod seconds 60)))
      (setq desc (s-concat desc (format "%ds" seconds))))
    desc))

(defun start-notify (message arg2)
  "Show MESSAGE as notification after some minutes or at some certain time of ARG2."
  (interactive
   (list
    (read-string "notification to send: " nil nil nil)
    (if (null current-prefix-arg)
        (read-number "after minitues: " 0)
      (read-string "at the time: " (current-time-normal-string)))))
  (let* ((current-time-stamp (current-timestamp))
         (minutes (if (null current-prefix-arg)
                      arg2 (/ (- (string-to-timestamp arg2)
                                 (current-timestamp))
                              60.0)))
         (timestamp (+ current-time-stamp (* 60.0 minutes))))
    (when (> minutes 0)
      (let* ((message (base64-encode-string-of-multibyte message))
             (notification `((timestamp . ,timestamp) (message . ,message)))
             (notify-string (base64-encode-string-of-multibyte (format "%s" notification))))
        (add-to-list '*notifications* notification)
        (do-start-notify notify-string minutes)))))

(defun load-notifications ()
  "."
  (when (file-exists-p +notifications-file-name+)
    (with-temp-buffer
      (insert-file-contents +notifications-file-name+)
      (goto-char (point-min))
      (let ((content (read (current-buffer))))
        (when (> (length content) 0)
          (setq *notifications* content)
          (remove-expired-notifications)
          (let ((now (current-timestamp)))
            (dolist (notification *notifications*)
              (let* ((timestamp (cdr (assq 'timestamp notification)))
                     (minutes (/ (- timestamp now) 60.0))
                     (notify-string (base64-encode-string-of-multibyte (format "%s" notification))))
                (message "start notify: %S" notification)
                (do-start-notify notify-string minutes))))))))
  t)

(defun remove-expired-notifications ()
  "."
  (let ((now (current-timestamp)))
    (setq *notifications*
          (remove-if
           (lambda (notification)
             (> now (cdr (assq 'timestamp notification))))
           *notifications*))))

(defun remove-the-notifications (timestamp)
  "Remove notification who's timestamp is TIMESTAMP."
  (setq *notifications*
        (remove-if
         (lambda (notification)
           (= timestamp (cdr (assq 'timestamp notification))))
         *notifications*)))

(defun save-notifications ()
  "."
  (remove-expired-notifications)
  (let ((content (format "%s" *notifications*)))
    (with-temp-file +notifications-file-name+
      (insert content))))

(defun do-start-notify (notification minutes)
  "Show message of NOTIFICATION after some MINUTES."
  (async-start
   `(lambda ()
      (let ((seconds (* 60 ,minutes)))
        (sleep-for seconds)
        ,notification))
   `(lambda (arg)
      (let* ((notification (base64-decode-string-as-multibyte arg))
             (notification (read notification)))
        (message "received notification: %S" notification)
        (let* ((message (format "%s" (cdr (assq 'message notification))))
               (message (base64-decode-string-as-multibyte message))
               (timestamp (cdr (assq 'timestamp notification))))
          (with-current-buffer (get-buffer-create *notifications-buffer*)
            (read-only-mode 0)
            (goto-char (point-max))
            (insert (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)) "\n" message "\n\n")
            (read-only-mode t))
          (display-buffer "*notification*")
          (remove-the-notifications timestamp))))))

(global-set-key (kbd "C-x s") 'start-notify)

(add-hook 'emacs-startup-hook
          #'(lambda ()
              ;; load content from file first
              (when (load-notifications)
                ;; add the save-notifications to 'kill-emacs-hook after load file success,
                ;;or it will save empty content to file dangerously.
                (add-hook 'kill-<emacs-hook 'save-notifications))))

(provide 'notifications)
;;; notifications.el ends here
