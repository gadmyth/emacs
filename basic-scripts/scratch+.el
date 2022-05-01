;;; scratch+.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2020 gadmyth

;; Author: scratch+.el <gadmyth@gmail.com>
;; Version: 1.0.1
;; Package-Version: 20220501.001
;; Package-Requires: dates
;; Keywords: scratch, auto save
;; Homepage: https://www.github.com/gadmyth/emacs
;; URL: https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/scratch+.el

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
;; scratch+'s code can be found here:
;;   https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/scratch+.el

;;; Commentary:
;;; Code:

(require 'dates)

(defvar +default-scratch-file-name+ (expand-file-name "~/.emacs.scratch"))

(defvar *last-scratch-buffer-size* -1)

(defvar *scratch-autosave-timer* nil)

(defvar *scratch-autosave-idle-timer* nil)

(defvar *scratch-autosave-interval* (* 10 60))

(defconst +scratch-autosave-interval-step+ (* 10 60))

(defvar *scratch-autosave-idle-delay* (* 3 60))

(defvar *scratch-next-check-time* nil)

(defvar *scratch-autosave-status* nil)

(defvar *scratch-last-saved-time* nil)

(add-to-list 'auto-coding-alist '("\\.scratch\\'" . utf-8))

(defun scratch-autosave-saving-p ()
  "."
  (eq *scratch-autosave-status* 'saving))

(defun scratch-autosave-loading-p ()
  "."
  (eq *scratch-autosave-status* 'loading))

(defun scratch-autosave-init-p ()
  "."
  (eq *scratch-autosave-status* nil))

(defun scratch-reset-autosave-status ()
  "."
  (setq *scratch-autosave-status* nil))

(defun scratch-buffer-size ()
  "."
  (let* ((buffer (get-buffer "*scratch*"))
         (size (buffer-size buffer)))
    size))

(defun scratch-mark-buffer-size ()
  "."
  (setq *last-scratch-buffer-size* (scratch-buffer-size)))

(defun scratch-buffer-size-changed-p ()
  "."
  (let* ((size (scratch-buffer-size))
         (size-changed-p (not (eq *last-scratch-buffer-size* size))))
    size-changed-p))

(defun load-scratch-from-file (&optional override)
  "Load scratch file to *scratch* buffer, if OVERRIDE is t, erase buffer first."
  (interactive)
  (let ((loading-success-p))
    (cond
     ;; don't load while saving
     ((scratch-autosave-saving-p)
      (message "Can't load %s file, for now is saving *scratch* buffer to it!" +default-scratch-file-name+)
      nil)
     ;; don't load again while loading file
     ((scratch-autosave-loading-p)
      (message "Can't load %s file, for now is already loading into *scratch* buffer!" +default-scratch-file-name+)
      nil)
     ;; check the file exists or not
     ((not (file-exists-p +default-scratch-file-name+))
      (message "Can't load %s file, for it does not exist!" +default-scratch-file-name+)
      ;; set loading to success at the very beginning
      (setq loading-success-p t))
     (t
      (let ((buffer (get-buffer-create "*scratch*")))
        ;; lock the status to loading
        (setq *scratch-autosave-status* 'loading)
        (with-current-buffer
            buffer
          (if override (erase-buffer))
          (insert-file-contents +default-scratch-file-name+))
        ;; unlock the status
        (scratch-reset-autosave-status)
        ;; mark buffer size
        (scratch-mark-buffer-size)
        ;; set the loading result to success
        (setq loading-success-p t))))
    ;; return the loading result
    loading-success-p))

(defun start-scratch-autosave-timed-checker ()
  "Start a timed checker to auto save scratch buffer."
  (let ((timer *scratch-autosave-timer*))
    (cond
     ((scratch-autosave-init-p)
      (cond
       ;; timer not exist, create a new one
       ((not timer)
        (message "Now start the *scratch-autosave-timer*")
        (setq *scratch-autosave-timer*
              (run-with-timer
               *scratch-autosave-interval*
               nil
               #'save-scratch-buffer)))
       ;; timer is not activated, set time and re-activate
       ((not (or (memq timer timer-list)
                 (memq timer timer-idle-list)))
        (message "*scratch-autosave-timer* is not activated, now re-activate it!")
        (let* ((next-check-timestamp (+ *scratch-autosave-interval* (current-timestamp)))
               (next-check-time (time-convert next-check-timestamp 'list)))
          (timer-set-time timer next-check-time)
          (timer-activate timer)))
       ;; timer is activated
       (t
        (message "*scratch-autosave-timer* is already activated, don't activate again!"))))
     (t
      (message "*scratch-autosave-status* [%S] is not init status, can't schedule again!"
               *scratch-autosave-status*)))))

(defun start-scratch-autosave-idle-checker ()
  "Start a idle checker to auto save scratch buffer."
  (unless *scratch-autosave-idle-timer*
    (setq *scratch-autosave-idle-timer*
          (run-with-idle-timer
           *scratch-autosave-idle-delay*
           *scratch-autosave-idle-delay*
           #'idle-try-save-scratch-buffer))))

(defun scratch-buffer-status-check ()
  "."
  (cond
   ;; don't save when saving to file
   ((scratch-autosave-saving-p)
    (message "*** %s Can't save *scratch*, for now is saving it to %s!" now +default-scratch-file-name+)
    t)
   ;; don't save when loading file
   ((scratch-autosave-loading-p)
    (message "*** %s Can't save *scratch*, for now is loading %s into it!" now +default-scratch-file-name+)
    t)
   ;; check the buffer exists or not
   ((not (get-buffer "*scratch*"))
    (message "buffer <*scratch*> does not exist!")
    t)
   (t
    ;; other status
    nil)))

(defun do-save-scratch-buffer ()
  "Lock status to save buffer, and unlock status after saved."
  ;; lock the status to saving
  (setq *scratch-autosave-status* 'saving)
  (with-current-buffer buffer
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (with-temp-file +default-scratch-file-name+
        (insert content))))
  ;; update saved time
  (setq *scratch-last-saved-time* now)
  ;; unlock the status
  (scratch-reset-autosave-status)
  ;; mark buffer size
  (scratch-mark-buffer-size))

(defun idle-try-save-scratch-buffer ()
  "Try to save scratch buffer at Emacs's idle time."
  (let* ((current-timestamp (current-timestamp))
         (diff (- *scratch-next-check-time* current-timestamp)))
    (when (> diff +scratch-autosave-interval-step+)
      (message "### %s try to save scratch buffer at idle time." (current-time-normal-string))
      (let ((now (current-time-normal-string))
            (last-saved-time *scratch-last-saved-time*)
            (buffer (get-buffer "*scratch*")))
        (cond
         ;; check common status
         ((scratch-buffer-status-check))
         ;; check the buffer changed or not
         ((not (scratch-buffer-size-changed-p))
          (message "### %s *scatch* buffer size not changed, last changed time [%s]"
                   now *scratch-last-saved-time*))
         (t
          (do-save-scratch-buffer)
          (message "### %s *scratch* buffer saved to file %s, last saved time: [%s]"
                   now +default-scratch-file-name+ last-saved-time)))))))

(defun save-scratch-buffer ()
  "Save *scatch* buffer to file."
  (interactive)
  (let ((now (current-time-normal-string))
        (last-saved-time *scratch-last-saved-time*)
        (buffer (get-buffer "*scratch*")))
    (cond
     ;; check common status
     ((scratch-buffer-status-check))
     ;; check the buffer changed or not
     ((not (scratch-buffer-size-changed-p))
      ;; increase the auto save interval step by step
      (when (< *scratch-autosave-interval* 3600)
        (setq *scratch-autosave-interval* (+ *scratch-autosave-interval* 600)))
      (setq *scratch-next-check-time* (+ *scratch-autosave-interval* (current-timestamp)))
      (let ((next-check-time-string (timestamp-to-normal-string *scratch-next-check-time*)))
        (message "**** %s *scatch* buffer size not changed, last changed time [%s], next check time: [%s]"
                 now *scratch-last-saved-time* next-check-time-string))
      ;; schedule next check task
      (start-scratch-autosave-timed-checker))
     (t
      (do-save-scratch-buffer)
      ;; revert the auto save interval
      (setq *scratch-autosave-interval* 600)
      (setq *scratch-next-check-time* (+ *scratch-autosave-interval* (current-timestamp)))
      (let ((next-check-time-string (timestamp-to-normal-string *scratch-next-check-time*)))
        (message "**** %s *scratch* buffer saved to file %s, last saved time: [%s], next check time: [%s]"
                 now +default-scratch-file-name+ last-saved-time next-check-time-string))
      ;; schedule next check task
      (start-scratch-autosave-timed-checker)))))

(defun scratch+-startup-hook ()
  "."
  ;; load content from file first
  (when (load-scratch-from-file t)
    ;; add the save-scratch-buffer to 'kill-emacs-hook after load file success,
    ;;or it will save empty content to file dangerously.
    (add-hook 'kill-emacs-hook 'save-scratch-buffer)
    ;; start a timed checker to save buffer to file
    (start-scratch-autosave-timed-checker)
    ;; start a idle checker to save buffer to file
    (start-scratch-autosave-idle-checker)))

(add-hook 'emacs-startup-hook #'scratch+-startup-hook)

(provide 'scratch+)
;;; scratch+.el ends here
