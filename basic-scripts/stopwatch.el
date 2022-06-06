;;; stopwatch.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2022 gadmyth

;; Author: stopwatch.el <gadmyth@gmail.com>
;; Version: 1.0.9
;; Package-Version: 20220606.001
;; Package-Requires: switch-buffer-functions, dates, dash
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
(require 'dash)

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
              (max-file-table)
              (switch-file-table)
              (ext-table)
              (max-ext-table)
              (switch-ext-table))
          (while (re-search-forward "^[[:digit:]]*\t.*\t.\t\\(.*\\)\t\\([[:digit:]]+\\)$" nil t 1)
            (let* ((buffer-name (match-string-no-properties 1))
                   (extension (file-name-extension buffer-name t))
                   (interval (string-to-number (match-string-no-properties 2))))
              (stopwatch-debug-message "find %s's time interval: %d" buffer-name interval)
              (when (cond ((zerop (length filter)) t)
                          (t (string-match-p filter buffer-name)))
                (cl-incf sum interval)
                ;; sum file interval
                (let ((tik (or (alist-str-get buffer-name file-table) 0)))
                  (alist-str-set buffer-name file-table (+ tik interval))
                  ;; switch count
                  (when (> tik 0)
                    (let ((count (or (alist-str-get buffer-name switch-file-table) 0)))
                      (alist-str-set buffer-name switch-file-table (1+ count)))))
                ;; max file interval
                (let ((tik (or (alist-str-get buffer-name max-file-table) 0)))
                  (when (> interval tik)
                    (alist-str-set buffer-name max-file-table interval)))
                ;; sum ext interval
                (let ((ext (if (> (length extension) 0) extension buffer-name)))
                  (let ((tik (or (alist-str-get ext ext-table) 0)))
                    (alist-str-set ext ext-table (+ tik interval))
                    ;; switch count
                    (when (> tik 0)
                      (let ((count (or (alist-str-get ext switch-ext-table) 0)))
                        (alist-str-set ext switch-ext-table (1+ count))))))
                ;; max ext interval
                (let ((ext (if (> (length extension) 0) extension buffer-name)))
                  (let ((tik (or (alist-str-get ext max-ext-table) 0)))
                    (when (> interval tik)
                      (alist-str-set ext max-ext-table interval)))))))
          (let ((date (replace-regexp-in-string "\\." "" (replace-regexp-in-string *stopwatch-log-file* "" filename)))
                (hours (/ sum 3600.0))
                (headers '("file" "duration" "percent" "max duration" "max percent" "switch count"))
                (rows))
            (setq rows (append rows
                               (stopwatch-statistic-for-table file-table max-file-table switch-file-table)
                               (list (vector "" "" "" "" "" ""))
                               (stopwatch-statistic-for-table ext-table max-ext-table switch-ext-table)
                               (list (vector "" "" "" "" "" ""))
                               (list (vector "total" (format "%.2fh" hours) "" "" "" ""))))
            (message "*** The total time using emacs on %s with filter [%s] is %.4f hours ***" date filter hours)
            (display-table-in-buffer headers rows)))))))

(defmacro alist-str-get (key alist)
  "Get value of KEY from ALIST using `string-equal` compare func."
  `(alist-get ,key ,alist nil nil #'string-equal))

(defmacro alist-str-set (key alist value)
  "Set VALUE for KEY of ALIST using `string-equal` compare func."
  `(setf (alist-get ,key ,alist nil nil #'string-equal) ,value))

(defun stopwatch-statistic-for-table (table max-table switch-table)
  "Show statistic message from TABLE, MAX-TABLE and SWITCH-TABLE."
  (let ((rows))
    (dolist (stat (sort table (lambda (a b)
                                (>= (cdr a) (cdr b)))))
      (let* ((key (car stat))
             (duration (or (cdr stat) 0))
             (cost (stopwatch-calc-cost duration))
             (cost-percent (format "%.2f%%" (/ (* 100.0 duration) sum)))
             (max-duration (or (alist-str-get key max-table) 0))
             (max-cost (stopwatch-calc-cost max-duration))
             (max-cost-percent (format "%.2f%%" (/ (* 100.0 max-duration) duration)))
             (switch-count (format "%d" (or (alist-str-get key switch-table) 0))))
        (message "%s: %s, %s, max: %s, %s, %s" key cost cost-percent max-cost max-cost-percent switch-count)
        (push (vector key cost cost-percent max-cost max-cost-percent switch-count) rows)))
    (reverse rows)))

(defun stopwatch-calc-cost (second)
  "Calculate human readable cost of SECOND."
  (let ((cost (cond
               ((> second 3600)
                (format "%.2fh" (/ second 3600.0)))
               ((> second 60)
                (format "%.2fm" (/ second 60.0)))
               (t
                (format "%ds" second)))))
    cost))

(defun make-tabulated-headers (column-names rows)
  "Column width are calculated by picking the max width of every cell under the COLUMN-NAMES and ROWS."
  "Copied from http://rgrinberg.com/posts/emacs-table-display/"
  (let ((widths
         (-reduce-from
          (lambda (acc x)
            (-zip-with (lambda (l r) (max l (length r))) acc (append x '())))
          (-map #'length columns-names)
          rows)))
    (cl-map
     #'vector #'identity
     (-zip-with
      (lambda (col size) (list col size nil))
      columns-names widths))))

(defun display-table-in-buffer (columns-names rows)
  "Display table in a buffer with COLUMNS-NAMES and ROWS."
  "Copied from http://rgrinberg.com/posts/emacs-table-display/"
  (let ((headers (make-tabulated-headers columns-names rows))
        (bname "*display table*"))
    (with-current-buffer (get-buffer-create bname)
      (tabulated-list-mode)
      (setq tabulated-list-format headers)
      (setq tabulated-list-padding 2)
      (tabulated-list-init-header)
      (setq tabulated-list-entries
            (-zip-with
             (lambda (i x) (list i x))
             (-iterate '1+ 0 (length rows))
             rows))
      (tabulated-list-print t)
      (display-buffer bname))))

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
  (let ((frames (frame-list))
        (active-frame (stopwatch-active-frame)))
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
  (let ((frames (frame-list))
        (active-frame (stopwatch-active-frame)))
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
