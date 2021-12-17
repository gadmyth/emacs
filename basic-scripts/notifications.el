;;; notifications.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2021 gadmyth

;; Author: notifications.el <gadmyth@gmail.com>
;; Version: 1.1.0
;; Package-Version: 20211217.001
;; Package-Requires: timer, dates, codec
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


(require 'timer)
(require 'dates)
(require 'codec)
(require 'uuid)

(defvar *notifications* nil)

(defvar *notifications-buffer* "*notifications*")

(defvar +notifications-file-name+ (expand-file-name "~/.emacs.notifications"))

(defun list-notifications ()
  "."
  (interactive)
  (let* ((notifications (mapcar (lambda (notification)
                                  (let* ((message (alist-get 'message notification))
                                         (message (base64-decode-string-as-multibyte message))
                                         (fire-time (alist-get 'fire-time notification))
                                         (fire-time-str (timestamp-to-normal-string fire-time)))
                                    (format "%s (%s): %s" fire-time-str (notification-time-diff-description fire-time) message)))
                                *notifications*))
         (notifications (sort notifications #'string<))
         (current-timestr (current-time-normal-string)))
    (completing-read (format "The notifications (%s): " current-timestr) notifications)))

(defun notification-time-diff-description (fire-time)
  "Show the time diff description of FIRE-TIME and now."
  (let* ((now (current-timestamp))
         (diff (- fire-time now))
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

(defun start-notify (message arg2 arg3)
  "Show MESSAGE as notification after some minutes or at some certain time of ARG2, ARG3."
  (interactive
   (list
    (read-string "notification to send: " nil nil nil)
    (if (null current-prefix-arg)
        (read-number "after minitues: " 0)
      (read-string "at the time: " (current-time-normal-string)))
    (read-string "repeat duration: " "0m")))
  (let* ((now (current-timestamp))
         (seconds (if (null current-prefix-arg)
                      (* 60 arg2)
                    (- (string-to-timestamp arg2) now))))
    (when (> seconds 0)
      (let* ((message (base64-encode-string-of-multibyte message))
             (fire-time (+ now seconds))
             (index (- (length arg3) 1))
             (repeat-duration-number (string-to-number (substring arg3 0 index)))
             (repeat-duration-unit (substring arg3 index))
             (repeat-duration (pcase repeat-duration-unit
                                ("m" (* 60 repeat-duration-number))
                                ("h" (* 60 60 repeat-duration-number))
                                ("d" (* 24 60 60 repeat-duration-number))
                                ("w" (* 7 24 60 60 repeat-duration-number))
                                ("M" (* 30 24 60 60 repeat-duration-number))
                                (_ repeat-duration-number)))
             (id (replace-regexp-in-string "-" "" (uuid-string)))
             (notification `((id . ,id)
                             (fire-time . ,fire-time)
                             (message . ,message)
                             (repeat-duration . ,repeat-duration))))
        (update-notification notification)
        (do-schedule-notification id seconds)))))

(defun load-notifications ()
  "."
  (when (file-exists-p +notifications-file-name+)
    (with-temp-buffer
      (insert-file-contents +notifications-file-name+)
      (goto-char (point-min))
      (let ((content (read (current-buffer))))
        (when (> (length content) 0)
          (remove-expired-notifications)
          (let ((now (current-timestamp)))
            (dolist (notification content)
              (load-notification notification)))))))
  t)

(defun load-notification (notification)
  "Load the NOTIFICATION if not exist in *notifications* list."
  (let* ((id (alist-get 'id notification))
         (noti (get-notification id)))
    (unless noti
      (setf (get-notification id) notification)
      (schedule-notification id))))

(defun schedule-notification (id)
  "Schedule the notification of ID."
  (let* ((notification (get-notification id))
         (scheduled (alist-get 'scheduled notification))
         (now (current-timestamp))
         (fire-time (alist-get 'fire-time notification)))
    (when (or
           ;; scheduled and not fired in the past
           (and scheduled
                (< fire-time now))
           ;; not scheduled and should be fired in the future
           (and (not scheduled)
                (>= fire-time now)))
      (let ((seconds (- fire-time now)))
        (message "now schedule the notification: %S" notification)
        (do-schedule-notification id seconds)))))

(defun remove-expired-notifications ()
  "."
  (let ((now (current-timestamp)))
    (dolist (noti *notifications*)
      (let* ((id (car noti))
             (notification (cdr noti))
             (fire-time (alist-get 'fire-time notification))
             (repeat-duration (alist-get 'repeat-duration notification)))
        (when (and (> now fire-time)
                   (= repeat-duration 0.0))
          (setf (get-notification id) nil))))))

(defun remove-fired-notifications ()
  "Remove FIRED notifications."
  (dolist (noti *notifications*)
    (let* ((id (car noti))
           (notification (cdr noti))
           (fired (alist-get 'fired notification))
           (repeat-duration (alist-get 'repeat-duration notification)))
      (when (and fired
                 (= repeat-duration 0.0))
        (setf (get-notification id) nil)))))

(defun refresh-repeatable-notifications ()
  "."
  (let ((now (current-timestamp)))
    (dolist (notification *notifications*)
      (refresh-repeatable-notification notification))))

(defun refresh-repeatable-notification (id)
  "Refresh the fire time of the fired repeatable notification of ID."
  (let* ((notification (get-notification id))
         (fire-time (alist-get 'fire-time notification))
         (repeat-duration (alist-get 'repeat-duration notification))
         (fired (alist-get 'fired notification))
         (now (current-timestamp)))
    (when (and fired
               (>= now fire-time)
               repeat-duration
               (> repeat-duration 0.0))
      (let* ((multiple (floor (/ (- now fire-time) repeat-duration)))
             (new-fire-time (+ fire-time (* (+ multiple 1) repeat-duration))))
        ;; refresh the new fire time
        (set-notify-property notification 'fire-time new-fire-time)
        ;; reset the scheduled mark
        (set-notify-property notification 'scheduled nil)
        ;; reset the fired mark
        (set-notify-property notification 'fired nil)
        ;; update notification back to list
        (update-notification notification)
        ;; re-schedule the repeatable notification
        (schedule-notification id)))))

(defun save-notifications ()
  "."
  (remove-expired-notifications)
    (with-temp-file +notifications-file-name+
      (print *notifications* (current-buffer))))

(defun do-schedule-notification (id seconds)
  "Show notification message which id is ID after some SECONDS."
  (when (run-with-timer
         seconds
         nil
         `(lambda ()
            (let ((notification (get-notification ,id)))
              (message "prepare to fire notification: %S" notification)
              (let* ((message (alist-get 'message notification))
                     (message (base64-decode-string-as-multibyte message))
                     (id (alist-get 'id notification))
                     (fire-time (alist-get 'fire-time notification))
                     (buffer (get-buffer-create *notifications-buffer*)))
                (with-current-buffer buffer
                  (read-only-mode 0)
                  (goto-char (point-max))
                  (insert (format-time-string "%Y-%m-%d %H:%M:%S" fire-time) "\n" message "\n\n")
                  (read-only-mode t))
                (display-buffer buffer)
                ;; mark as fired
                (set-notify-property notification 'fired t)
                (update-notification notification)
                ;; re-schedule repeatable notification
                (refresh-repeatable-notification id)
                (remove-fired-notifications)))))
    (let ((notification (get-notification id)))
      (set-notify-property notification 'scheduled t)
      (update-notification notification)
      (message "scheduled notification: %S" notification))))

(defun update-notification (notification)
  "NOTIFICATION."
  (when-let ((id (alist-get 'id notification)))
    (setf (get-notification id) notification)))

(defmacro set-notify-property (notification key value)
  "Update NOTIFICATION's VALUE of KEY."
  `(setf (alist-get ,key ,notification) ,value))

(defmacro get-notification (id)
  "ID."
  `(alist-get ,id *notifications* nil t 'equal))

(global-set-key (kbd "C-x s") 'start-notify)

(add-hook 'emacs-startup-hook
          #'(lambda ()
              ;; load content from file first
              (when (load-notifications)
                ;; add the save-notifications to 'kill-emacs-hook after load file success,
                ;;or it will save empty content to file dangerously.
                (add-hook 'kill-emacs-hook 'save-notifications))))


(provide 'notifications)
;;; notifications.el ends here
