;;; notifications.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2021 gadmyth

;; Author: notifications.el <gadmyth@gmail.com>
;; Version: 1.1.10
;; Package-Version: 20220110.001
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

(defvar *notification-timers* nil)

(defvar *notifications-buffer* nil)

(defvar +notifications-file-name+ (expand-file-name "~/.emacs.notifications"))

(defvar *notification-actions*`(("fire" . fire-notification)
                                 ("cancel" . cancel-notification)
                                 ("reset" . reset-notification)))

(defvar *notification-buffer-actions*
  `(("reschedule at" . reschedule-notification-at)
    ("reschedule after" . reschedule-notification-after)))

(defface notifications-header-face '((t :weight bold))
  "Face used for header in p2p notifications buffer.

Buttons will be displayed in this face when the mouse cursor is
above them."
  :group 'notifications-faces)

(defmacro get-notification (id)
  "ID."
  `(alist-get ,id *notifications* nil t 'equal))

(defmacro get-notification-timer (id)
  "ID."
  `(alist-get ,id *notification-timers* nil t 'equal))

(defun list-notifications ()
  "."
  (interactive)
  (let* ((tomorrow-timestamp (tomorrow-timestamp))
         (notifications (if current-prefix-arg
                            *notifications*
                          ;; filter the today's notifications
                          (seq-filter(lambda (notification)
                                       (> tomorrow-timestamp (alist-get 'fire-time notification)))
                                     *notifications*)))
         (notifications (mapcar
                         (lambda (notification)
                           (let* ((message (alist-get 'message notification))
                                  (message (base64-decode-string-as-multibyte message))
                                  (fire-time (alist-get 'fire-time notification))
                                  (fire-time-str (timestamp-to-normal-string fire-time))
                                  (diff (notification-time-diff-description fire-time))
                                  (content (format "%s (%s): %s" fire-time-str diff message))
                                  (id (alist-get 'id notification)))
                             (cons content id)))
                         notifications))
         (notifications (sort notifications (lambda (pair1 pair2) (string< (car pair1) (car pair2)))))
         (current-timestr (current-time-normal-string))
         (content (completing-read (format "The notifications (%s): " current-timestr) notifications))
         (id (alist-get content notifications nil nil #'equal)))
    (message "Select the notification of id: %S" id)
    (let* ((action-name (completing-read "Choose the notification action: " *notification-actions*))
           (action (alist-get action-name *notification-actions* nil nil #'equal)))
      (message "The action choosed: %S" action)
      (funcall action id))))

(defun notification-time-diff-description (fire-time)
  "Show the time diff description of FIRE-TIME and now."
  (let* ((now (current-timestamp))
         (diff (- fire-time now))
         (sign (if (> diff 0) "+" "-"))
         (diff (abs diff))
         (seconds diff)
         (minutes (/ diff 60))
         (hours (/ diff 3600))
         (days (/ diff 86400))
         (months (/ diff (* 86400 30)))
         (years (/ diff (* 86400 365)))
         (desc ""))
    (when (>= years 1)
      (setq desc (s-concat desc (format "%dy" years))))
    (when (>= months 12)
      (setq months (mod months 12)))
    (when (>= months 1)
      (setq desc (cat desc (format "%dm" months))))
    (when (>= days 30)
      (setq days (mod days 30)))
    (when (>= days 1)
      (setq desc (s-concat desc (format "%dd" days))))
    (when (>= hours 24)
        (setq hours (mod hours 24)))
    (when (>= hours 1)
      (setq desc (s-concat desc (format "%dh" hours))))
    (when (>= minutes 60)
      (setq minutes (mod minutes 60)))
    (when (>= minutes 1)
      (setq desc (s-concat desc (format "%dm" minutes))))
    (when (>= seconds 60)
      (setq seconds (mod seconds 60)))
    (when (>= seconds 1)
      (setq desc (s-concat desc (format "%ds" seconds))))
    (setq desc (format "%s%s" sign desc))
    desc))

(defun start-notify (message arg2 arg3)
  "Show MESSAGE as notification after some minutes or at some certain time of ARG2, ARG3."
  (interactive
   (list
    (read-string "notification to send: " nil nil nil)
    (if current-prefix-arg
        (read-number "after minitues: " 0)
      (read-string "at the time: " (current-time-normal-string)))
    (read-string "repeat duration: " "0m")))
  (let* ((now (current-timestamp))
         (seconds (if current-prefix-arg
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
      (let ((list (read (current-buffer))))
        (when (> (length list) 0)
          (remove-expired-notifications)
          (let ((now (current-timestamp)))
            (dolist (element list)
              (let ((notification (cdr element)))
                (load-notification notification))))))))
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

(defun reschedule-notification-at (notification)
  "Reschedule the expired NOTIFICATION at the certain future time."
  (let* ((time-str (read-string "at the time: " (current-time-normal-string)))
         (fire-time (string-to-timestamp time-str)))
    (set-notify-property notification 'fire-time fire-time)
    (reschedule-notifitaion notification)))

(defun reschedule-notification-after (notification)
  "Reschedule the expired NOTIFICATION after the certain duration."
  (let* ((duration-str (read-string "repeat duration: " "0m"))
         (index (- (length duration-str) 1))
         (duration-number (string-to-number (substring duration-str 0 index)))
         (duration-unit (substring duration-str index))
         (duration (pcase duration-unit
                     ("s" duration-number)
                     ("m" (* 60 duration-number))
                     ("h" (* 60 60 duration-number))
                     ("d" (* 24 60 60 duration-number))
                     ("w" (* 7 24 60 60 duration-number))
                     ("M" (* 30 24 60 60 duration-number))
                     (_ duration-number)))
         (now (current-timestamp))
         (fire-time (+ now duration)))
    (set-notify-property notification 'fire-time fire-time)
    (reschedule-notifitaion notification)))

(defun reschedule-notifitaion (notification)
  "Reschedule the NOTIFICATION."
  (let ((id (alist-get 'id notification)))
    (set-notify-property notification 'scheduled nil)
    (set-notify-property notification 'fired nil)
    (setf (get-notification id) notification)
    (schedule-notification id)))

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
  (when-let ((timer (run-with-timer
                     seconds
                     nil
                     `(lambda ()
                        (fire-notification ,id)))))
    (let ((notification (get-notification id)))
      (set-notify-property notification 'scheduled t)
      (update-notification notification)
      (setf (get-notification-timer id) timer)
      (message "scheduled notification: %S" notification))))

(defun fire-notification (id)
  "Fire the notification of ID right now."
  (let ((notification (get-notification id)))
    (message "prepare to fire notification: %S" notification)
    (let* ((message (alist-get 'message notification))
           (message (base64-decode-string-as-multibyte message))
           (id (alist-get 'id notification))
           (fire-time (alist-get 'fire-time notification))
           (buffer *notifications-buffer*))
      ;; create the buffer
      (when (not (buffer-live-p buffer))
        (setq *notifications-buffer* (generate-new-buffer "*notifications*"))
        ;; set the major mode
        (with-current-buffer *notifications-buffer* (notifications-aggregate-mode))
        (setq buffer *notifications-buffer*))
      ;; display
      (display-buffer buffer)
      ;; insert notification
      (with-current-buffer buffer
        (read-only-mode 0)
        (save-excursion
          (goto-char (point-max))
          (when (> (point-max) (point-min))
            (insert "\n\n"))
          (let* ((header (format-time-string "%Y-%m-%d %H:%M:%S" fire-time))
                 (header-start (point))
                 (header-end (+ header-start (length header))))
            (put-text-property 0 (length header)
                               'font-lock-face 'notifications-header-face header)
            (insert header "\n" message)
            (notifications-add-button header-start header-end notification)))
        (set-window-point (get-buffer-window buffer 'visible) (point-max))
        (read-only-mode t))
      ;; reset timer
      (setf (get-notification-timer id) nil)
      ;; mark as fired
      (set-notify-property notification 'fired t)
      (update-notification notification)
      ;; re-schedule repeatable notification
      (refresh-repeatable-notification id)
      (remove-fired-notifications))))

(defun update-notification (notification)
  "NOTIFICATION."
  (when-let ((id (alist-get 'id notification)))
    (setf (get-notification id) notification)))

(defun cancel-notification (id)
  "Cancel the notification of ID."
  (when-let ((timer (get-notification-timer id)))
    (cancel-timer timer))
  (setf (get-notification id) nil))

(defun reset-notification (id)
  "Reset the notification of ID.")

(defmacro set-notify-property (notification key value)
  "Update NOTIFICATION's VALUE of KEY."
  `(setf (alist-get ,key ,notification) ,value))

(defun notifications-add-button (start end notification)
  "Add a button property to from START to END with NOTIFICATION."
  (add-text-properties
   start end
   (nconc
    (list 'notification-callback #'notifications-button-callback)
    (list 'notification-data (list notification))
    (list 'rear-nonsticky t))))

(defun notifications-button-callback (data)
  "Callback of the notification button with DATA as args."
  (when-let ((notification (car data)))
    ;; DO NOTING
    ))

(defun notifications-header-p ()
  "."
  (eq (get-text-property (point) 'notification-callback)
      'notifications-button-callback
      ))

(defun notifications-current-notification ()
  "Goto the current notification's head in *notifications-buffer*."
  (interactive)
  (beginning-of-visual-line)
  (cl-loop while (and (not (notifications-header-p))
                      (not (eq (point) (point-min)))
                      (not (eq (point) (point-max))))
           do (previous-line)))

(defun notifications-next-notification ()
  "Goto the next notification's head in *notifications-buffer*."
  (interactive)
  (beginning-of-visual-line)
  (forward-line)
  (cl-loop while (and (not (notifications-header-p))
                      (not (eq (point) (point-min)))
                      (not (eq (point) (point-max))))
           do (forward-line)))

(defun notifications-previous-notification ()
  "Goto the previous notification's head in *notifications-buffer*."
  (interactive)
  (beginning-of-visual-line)
  (previous-line)
  (cl-loop while (and (not (notifications-header-p))
                      (not (eq (point) (point-min)))
                      (not (eq (point) (point-max))))
           do (previous-line)))

(defun notifications-delete-this-notification ()
  "Delete the notification in *notifications--buffer*."
  (interactive)
  (read-only-mode 0)
  (notifications-current-notification)
  (let ((msg-start (point)))
    (notifications-next-notification)
    (let ((msg-end (point)))
      (delete-region msg-start msg-end)))
  (read-only-mode 1))

(defun notifications-operate-this-notification ()
  "Delete the notification in *notifications--buffer*."
  (interactive)
  (notifications-current-notification)
  (let* ((data (get-text-property (point) 'notification-data))
         (notification (car data)))
    (message "operate on the notification: %S" notification)
    (let* ((action-name (completing-read "Choose the notification action: " *notification-buffer-actions*))
           (action (alist-get action-name *notification-buffer-actions* nil nil #'equal)))
      (message "The action choosed: %S" action)
      (funcall action notification))))

(defun notifications-undo ()
  "Undo in the *notifications-buffer*."
  (interactive)
  (read-only-mode 0)
  (undo)
  (read-only-mode 1))

(defvar notifications-aggregate-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map "n" 'notifications-next-notification)
    (define-key map "p" 'notifications-previous-notification)
    (define-key map "o" 'notifications-operate-this-notification)
    (define-key map "d" 'notifications-delete-this-notification)
    (define-key map "u" 'notifications-undo)
    (define-key map "q" 'quit-window)
    map))

(define-derived-mode notifications-aggregate-mode text-mode "NOTIA"
  ;; The mode for *notifications-buffer*.
  (use-local-map notifications-aggregate-mode-map))

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
