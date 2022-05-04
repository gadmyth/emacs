;;; stopwatch.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2022 gadmyth

;; Author: stopwatch.el <gadmyth@gmail.com>
;; Version: 1.0.6
;; Package-Version: 20220504.001
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
(defvar *stopwatch-log-file* (expand-file-name "stopwatch.log" "~/.emacs.stopwatch"))
(defvar *stopwatch-debug* nil)
(defvar *stopwatch-focus-changed* nil)
(defconst *stopwatch-log-version* 2)
(defvar *stopwatch-focus-change-callback-added* nil)

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
    (stopwatch-log "s")))

(defun stopwatch-log-file ()
  "."
  (format "%s.%s" *stopwatch-log-file* (current-time-short-string)))

(defun stopwatch-log (action &optional active-buffer)
  "Record stopwatch log to file with ACTION and ACTIVE-BUFFER."
  (when (or active-buffer *stopwatch-previous-buffer*)
    (let* ((timestamp (current-timestamp))
           (buffer (or active-buffer *stopwatch-previous-buffer*))
           (start-time (gethash buffer *stopwatch-hash* timestamp))
           (duration (- timestamp start-time))
           (time-string (timestamp-to-normal-string timestamp))
           (content (format "%d\t%s\t%s\t%s\t%d\n"
                            *stopwatch-log-version*
                            time-string
                            action
                            buffer
                            duration)))
      (write-region content nil (stopwatch-log-file) 'append))))

(defun stopwatch-ensure-log-directory ()
  "."
  (let ((log-directory (file-name-directory *stopwatch-log-file*)))
    (unless (file-exists-p log-directory)
      (make-directory log-directory))))

(defun stopwatch-statistic ()
  "Statistic stopwatch's file with FILENAME, filter buffer name with REGEXP."
  (interactive)
  (let ((filename (read-file-name "Please select the file: " (file-name-directory *stopwatch-log-file*)))
        (filter (read-string "Please input the file name filter: ")))
    (when (file-exists-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (goto-char (point-min))
        (let ((sum 0)
              (file-table)
              (ext-table))
          (while (re-search-forward "^[[:digit:]]*\t.*\t.\t\\(.*\\)\t\\([[:digit:]]+\\)$" nil t 1)
            (let* ((buffer-name (match-string-no-properties 1))
                   (extension (file-name-extension buffer-name t))
                   (interval (string-to-number (match-string-no-properties 2))))
              (stopwatch-debug-message "find %s's time interval: %d" buffer-name interval)
              (when (cond ((zerop (length filter)) t)
                          (t (string-match-p filter buffer-name)))
                (cl-incf sum interval)
                (let ((tik (or (alist-get buffer-name file-table nil nil #'string-equal) 0)))
                  (setf (alist-get buffer-name file-table nil nil #'string-equal) (+ tik interval)))
                (let ((ext (if (> (length extension) 0) extension buffer-name)))
                  (let ((tik (or (alist-get ext ext-table nil nil #'string-equal) 0)))
                    (setf (alist-get ext ext-table nil nil #'string-equal) (+ tik interval)))))))
          (let ((date (replace-regexp-in-string "\\." "" (replace-regexp-in-string *stopwatch-log-file* "" filename)))
                (hours (/ sum 3600.0)))
            (message "*** Now stat for the %s stat file ***" filename)
            (message "--- Now stat for file ---")
            (dolist (stat (sort file-table (lambda (a b)
                                             (>= (cdr a) (cdr b)))))
              (message "%s: %s, %.4f%%" (car stat) (stopwatch-calc-cost (cdr stat)) (/ (* 100.0 (cdr stat)) sum)))
            (message "--- Now stat for extension ---")
            (dolist (stat (sort ext-table (lambda (a b)
                                            (>= (cdr a) (cdr b)))))
              (message "%s: %s, %.4f%%" (car stat) (stopwatch-calc-cost (cdr stat)) (/ (* 100.0 (cdr stat)) sum)))
            (message "*** The total time using emacs on %s with filter [%s] is %.4f hours ***" date filter hours)))))))

(defun stopwatch-calc-cost (second)
  "Calculate human readable cost of SECOND."
  (let* ((second (cdr stat))
         (cost (if (> second 3600)
                   (format "%.4f hours" (/ second 3600.0))
                 (format "%.2f minutes" (/ second 60.0)))))
    cost))

(defun stopwatch-active-frame ()
  "."
  (let ((active-frame)
        (frames (reverse (frame-list))))
    (dolist (frame frames)
      (when (frame-focus-state frame)
        (setq active-frame frame)))
    active-frame))

(defun stopwatch-focus-change-callback ()
  "."
  (let ((active-frame (stopwatch-active-frame)))
    (stopwatch-debug-message "frame focus state: %S, current frame: %S, active frame: %S, current buffer: %S"
                             (frame-focus-state)
                             (cl-position (window-frame) frames)
                             (and active-frame (cl-position active-frame frames))
                             (buffer-name (current-buffer)))
    (cond
     ((not active-frame)
      (stopwatch-log "d" *stopwatch-current-buffer*)
      (puthash (current-buffer) (current-timestamp) *stopwatch-hash*)
      ;; clear the current buffer
      (setq *stopwatch-previous-buffer* nil)
      (setq *stopwatch-current-buffer* nil))
     (t
      (setq *stopwatch-current-buffer* (current-buffer))
      (puthash (current-buffer) (current-timestamp) *stopwatch-hash*)
      (stopwatch-log "a" *stopwatch-current-buffer*)))))

(defun stopwatch-delete-frame-callback (&optional frame)
  "Record stopwatch log when the FRAME is deleted."
  (let ((active-frame (stopwatch-active-frame)))
    (stopwatch-debug-message "current frame: %S, active frame: %S, current buffer: %S"
                             (cl-position (window-frame) frames)
                             (and active-frame (cl-position active-frame frames))
                             (buffer-name (current-buffer)))
    
    (stopwatch-log "d" *stopwatch-current-buffer*)
    (puthash (current-buffer) (current-timestamp) *stopwatch-hash*)
    ;; clear the current buffer
    (setq *stopwatch-previous-buffer* nil)
    (setq *stopwatch-current-buffer* nil)))

(defun stopwatch-add-hooks ()
  "."
  ;; add buffer switch callback
  (add-hook 'switch-buffer-functions #'buffer-maybe-changed)
  ;; add frame delete callback
  (add-hook 'delete-frame-functions #'stopwatch-delete-frame-callback)
  ;; add post-command-hook
  (add-hook 'post-command-hook #'switch-buffer-functions-run)
  ;; add focus change callback
  (when (not *stopwatch-focus-change-callback-added*)
    (add-function :after after-focus-change-function
                  #'stopwatch-focus-change-callback)
    (setq *stopwatch-focus-change-callback-added* t)))

(defun stopwatch-remove-hooks ()
  "."
  ;; remove buffer switch callback
  (remove-hook 'switch-buffer-functions #'buffer-maybe-changed)
  ;; reomve frame delete callback
  (remove-hook 'delete-frame-functions #'stopwatch-delete-frame-callback)
  ;; remove post-command-hook
  (remove-hook 'post-command-hook 'switch-buffer-functions-run)
  ;; add focus change callback
  (when *stopwatch-focus-change-callback-added*
    (remove-function after-focus-change-function
                     #'stopwatch-focus-change-callback)
    (setq *stopwatch-focus-change-callback-added* nil)))

(define-minor-mode stopwatch-mode
  "Record buffer change and focus change."
  :require 'switch-buffer-functions
  :global t
  (cond
   (stopwatch-mode
    (stopwatch-ensure-log-directory)
    (stopwatch-add-hooks))
   (t
    (stopwatch-remove-hooks))))

(provide 'stopwatch)
;;; stopwatch.el ends here
