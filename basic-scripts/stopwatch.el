;;; stopwatch.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2022 gadmyth

;; Author: stopwatch.el <gadmyth@gmail.com>
;; Version: 1.0.1
;; Package-Version: 20220430.002
;; Package-Requires: switch-buffer-functions, dates
;; Keywords: stopwatch
;; Homepage: https://www.github.com/gadmyth/emacs
;; URL: https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/stopwatch.el

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
;; stopwatch's code can be found here:
;;   https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/stopwatch.el

;;; Commentary:
;;; Code:

(require 'switch-buffer-functions)
(require 'dates)

(defvar *stopwatch-current-buffer* nil)
(defvar *stopwatch-previous-buffer* nil)
(defvar *stopwatch-log-file* (expand-file-name ".emacs.stopwatch_log" "~"))
(defvar *stopwatch-debug* nil)
(defvar *stopwatch-focus-changed* nil)

(defvar-local stopwatch-current-timestamp nil)
(defvar *stopwatch-hash* (make-hash-table))

(defun stopwatch-toggle-debug ()
  "."
  (interactive)
  (setq *stopwatch-debug* (not *stopwatch-debug*))
  (message "turn %s the *stopwatch-debug*" (if *stopwatch-debug* "on" "off")))

(defmacro stopwatch-debug-message (format-string &rest ARGS)
  "If debug is open, send message with FORMAT-STRING and ARGS."
  `(if *stopwatch-debug*
       (message ,format-string ,@ARGS)))

(defun minibuffer-name-p (buffer)
  "Check the BUFFER's name contain Minibuf or not."
  (string-prefix-p " *Minibuf" (buffer-name buffer)))

(defun dired-buffer-p (buffer)
  "Check the BUFFER's \"major-mode\" is dired-mode or not."
  (eq 'dired-mode
      (buffer-local-value 'major-mode buffer)))

(defun buffer-maybe-changed (prev curr)
  "Hook function when the \"effective\" buffer changed from PREV to CURR."
  (cl-assert (eq curr (current-buffer)))
  ;; previous buffer changed, set previous buffer
  (stopwatch-debug-message "----")
  (stopwatch-debug-message "*prev*: %S,\tprev: %S\n*curr*: %S,\tcurr: %S"
                           (buffer-name *stopwatch-previous-buffer*)
                           (buffer-name prev)
                           (buffer-name *stopwatch-current-buffer*)
                           (buffer-name curr))
  ;; current buffer changed, set current buffer, previous buffer and record log
  (when (and (not (minibuffer-name-p curr))
             (not (dired-buffer-p curr))
             (not (eq *stopwatch-current-buffer* curr)))
    (stopwatch-debug-message "----")
    (stopwatch-debug-message "stopwatch prev changed: %s -> %s"
                             (buffer-name *stopwatch-previous-buffer*)
                             (buffer-name *stopwatch-current-buffer*))
    (setq *stopwatch-previous-buffer* *stopwatch-current-buffer*)
    
    (stopwatch-debug-message "----")
    (stopwatch-debug-message "stopwatch curr changed: %s -> %s"
                             (buffer-name *stopwatch-current-buffer*)
                             (buffer-name curr))
    
    (setq *stopwatch-current-buffer* curr)
    (puthash *stopwatch-current-buffer* (current-timestamp) *stopwatch-hash*)
    
    (stopwatch-debug-message "----")
    (stopwatch-debug-message "stopwatch buffer changed: %s -> %s"
                             (buffer-name *stopwatch-previous-buffer*)
                             (buffer-name *stopwatch-current-buffer*))
    (stopwatch-debug-message "----")
    ;; record log
    (stopwatch-log "switch-buffer")))

(defun stopwatch-log-file ()
  "."
  (format "%s.%s" *stopwatch-log-file* (current-time-short-string)))

(defun stopwatch-log (action &optional active-buffer)
  "Record stopwatch log to file with ACTION and ACTIVE-BUFFER."
  (when *stopwatch-previous-buffer*
    (let* ((timestamp (current-timestamp))
           (buffer (or active-buffer *stopwatch-previous-buffer*))
           (start-time (gethash buffer *stopwatch-hash* 0))
           (duration (- timestamp start-time))
           (time-string (timestamp-to-normal-string start-time))
           (content (format "%s\t%s\t%s\t%d\n" time-string action buffer duration)))
      (write-region content nil (stopwatch-log-file) 'append))))

(defun stopwatch-focus-change-callback ()
  "."
  (let ((active-frame)
        (frames (reverse (frame-list))))
    (dolist (frame frames)
      (when (frame-focus-state frame)
        (setq active-frame frame)))
    (stopwatch-debug-message "frame focus state: %S, current frame: %S, active frame: %S, current buffer: %S"
                             (frame-focus-state)
                             (cl-position (window-frame) frames)
                             (and active-frame (cl-position active-frame frames))
                             (buffer-name (current-buffer)))
    (cond
     ((not active-frame)
      (when (eq *stopwatch-current-buffer* (current-buffer))
        (stopwatch-log "deactive" *stopwatch-current-buffer*)
        (puthash (current-buffer) (current-timestamp) *stopwatch-hash*)))
     (t
      (when (eq *stopwatch-current-buffer* (current-buffer))
        (stopwatch-log "active" *stopwatch-current-buffer*)
        (puthash (current-buffer) (current-timestamp) *stopwatch-hash*))))))

(add-hook 'switch-buffer-functions #'buffer-maybe-changed)

(add-function :after after-focus-change-function
              #'stopwatch-focus-change-callback)

(provide 'stopwatch)
;;; stopwatch.el ends here
