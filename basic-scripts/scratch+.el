;;; scratch+.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2020 gadmyth

;; Author: scratch+.el <gadmyth@gmail.com}>
;; Version: 1.0
;; Package-Version: 20200329.001
;; Package-Requires:
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


(defvar +default-scratch-file-name+ (expand-file-name "~/.emacs.scratch"))

(defvar *last-scratch-buffer-size* -1)

(defvar *scratch-autosave-timer* nil)

(defconst +scratch-autosave-interval+ (* 10 60))

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

(defun start-scratch-autosave-scheduler ()
  "Start a scheduler to auto save scratch buffer."
  (if *scratch-autosave-timer*
      (message "*scratch-autosave-timer* started, don't start again!")
    (scratch-reset-autosave-status)
    (message "Now start the *scratch-autosave-timer*")
    (setq *scratch-autosave-timer*
          (run-with-timer
           +scratch-autosave-interval+
           +scratch-autosave-interval+ #'save-scratch-buffer))))

(defun save-scratch-buffer ()
  "Save *scatch* buffer to file."
  (interactive)
  (let ((now (format-time-string "%Y-%m-%d %a %H:%M" (current-time)))
        (last-saved-time *scratch-last-saved-time*)
        (buffer (get-buffer "*scratch*")))
    (cond
     ;; don't save when saving to file
     ((scratch-autosave-saving-p)
      (message "Can't save *scratch*, for now is saving it to %s [%s]!" +default-scratch-file-name+ now))
     ;; don't save when loading file
     ((scratch-autosave-loading-p)
      (message "Can't save *scratch*, for now is loading %s into it [now]!" +default-scratch-file-name+ now))
     ;; check the buffer exists or not
     ((not buffer)
      (message "buffer <*scratch*> does not exist!"))
     ;; check the buffer changed or not
     ((not (scratch-buffer-size-changed-p))
      (message "**** ignore to save *scatch* buffer, buffer size not changed [%s], last changed time is [%s] ****" now *scratch-last-saved-time*))
     (t
      ;; mark buffer size
      (scratch-mark-buffer-size)
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
      (message "**** *scratch* buffer saved to file %s [%s], lasted saved time: [%s] ****" +default-scratch-file-name+ now last-saved-time)))))

(add-hook 'emacs-startup-hook
          #'(lambda ()
              ;; load content from file first
              (when (load-scratch-from-file t)
                  ;; add the save-scratch-buffer to 'kill-emacs-hook after load file success,
                  ;;or it will save empty content to file dangerously.
                  (add-hook 'kill-emacs-hook 'save-scratch-buffer)
                  ;; start a scheduler to save buffer to file
                  (start-scratch-autosave-scheduler))))

(provide 'scratch+)
;;; scratch+.el ends here
