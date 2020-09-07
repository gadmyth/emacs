;;; eyebrowse+.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2020 gadmyth

;; Author: eyebrowse+.el <gadmyth@gmail.com}>
;; Version: 1.0.4
;; Package-Version: 20200907.001
;; Package-Requires: eyebrowse, s, dash
;; Keywords: eyebrowse, eyebrowse+
;; Homepage: https://www.github.com/gadmyth/emacs
;; URL:  https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/eyebrowse+.el

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
;; eyebrowse+'s code can be found here:
;;   https://github.com/gadmyth/emacs/blob/master/basic-scripts/eyebrowse+.el

;;; Commentary:
;;; Code:

(require 'eyebrowse)
(require 's)
(require 'dash)

(defvar *eyebrowse-debug* nil)
(defvar +eyebrowse-file-name+ (expand-file-name "~/.eyebrowse_save"))
(defvar *eyebrowse-default-configs* nil)
(defvar eyebrowse-lazy-load-hook nil)

(add-to-list 'auto-coding-alist '("\\.eyebrowse_save\\'" . utf-8))

(defun eyebrowse-lazy-load-config ()
  "."
  (run-with-timer
   1 nil
   (lambda ()
     ;; load eyebrowse config from file after 1 second
     (when (load-eyebrowse-config)
       ;; add the save function if loading success from file,
       ;; or it will be dangerous to overwrite the config to file.
       (run-hooks 'eyebrowse-lazy-load-hook)))))

(defmacro eyebrowse-message (format-string &rest ARGS)
  "If debug is open, send message with FORMAT-STRING and ARGS."
  `(if *eyebrowse-debug*
       (message ,format-string ,@ARGS)))

(defun eyebrowse-get-current-config ()
  "."
  (let* ((slot (eyebrowse--get 'current-slot))
         (config (eyebrowse-get-config-with-slot slot)))
    config))

(defun eyebrowse-get-last-config ()
  "."
  (let* ((slot (eyebrowse--get 'last-slot))
         (config (eyebrowse-get-config-with-slot slot)))
    config))

(defun eyebrowse-get-config-with-slot (slot)
  "SLOT: ."
  (let* ((configs (eyebrowse--get 'window-configs))
         (config (assq slot configs)))
    config))

(defun eyebrowse-current-config-string ()
  "."
  (let ((current-config (eyebrowse-get-current-config)))
    (eyebrowse-config-string current-config)))

(defun eyebrowse-config-string (config)
  "Get CONFIG string."
  (if (not config)
      nil
    (let* ((slot (car config))
	   (tag (nth 2 config)))
      (eyebrowse--config-string slot tag))))

(defun eyebrowse--config-string (slot tag)
  "SLOT, TAG."
  (let* ((slot-string (int-to-string slot))
         (ret slot-string))
    (if (> (length tag) 0)
        (setq ret (format "%s: %s" slot-string tag))
      ret)))

(defun eyebrowse-config-slot (config-string)
  "CONFIG-STRING."
  (if (s-contains? ":" config-string)
      (string-to-number (first (split-string config-string ":")))
    (string-to-number config-string)))

(defun eyebrowse-list-configs (&rest _)
  "."
  (interactive)
  (eyebrowse-list-window-configs (eyebrowse--get 'window-configs) nil))

(defun eyebrowse-list-configs-with-action (action)
  "ACTION: ."
  (interactive)
  (eyebrowse-list-window-configs-with-action (eyebrowse--get 'window-configs) nil action))

(defun eyebrowse-list-window-configs (configs buffer)
  "CONFIGS, BUFFER."
  (eyebrowse-list-window-configs-with-action
   configs buffer
   (lambda (element)
     (if (not (-contains? collections element))
         (eyebrowse-message "eyebrowse config %S does not exist!" element)
       (let* ((slot (eyebrowse-config-slot element))
              (window-configs (eyebrowse--get 'window-configs))
              (config (assq slot window-configs)))
         (select-buffer-window-safely-at-config buffer config))))))

(defun eyebrowse-list-window-configs-with-action (window-configs &optional buffer action)
  "WINDOW-CONFIGS: , BUFFER: , ACTION."
  (let* ((current-config (eyebrowse-get-current-config))
         (current-tag (nth 2 current-config))
         (default-candidate (eyebrowse-config-string current-config))
         (prompt (format "Select eyebrowse action (%s): " current-tag))
         (collections))
    (dolist (window-config window-configs)
      (let* ((slot (nth 0 window-config))
             (tag (nth 2 window-config))
             (element (eyebrowse-config-string window-config)))
        (eyebrowse-message "window config: %S" element)
        (push element collections)))
    (setf collections (reverse collections))
    (eyebrowse-message "collections: %S" collections)
    (let ((selected (completing-read prompt collections nil t nil nil default-candidate)))
      (funcall action selected))))

(defun select-buffer-window-safely-at-config (buffer &optional config)
  "Switch to window CONFIG, and select BUFFER."
  (let* ((locked-config (eyebrowse-get-lock-buffer-config buffer))
         (locked-slot (car locked-config))
         (target-config (or config locked-config))
         (locked-conf-string (eyebrowse-config-string locked-config))
         (target-slot (car target-config))
         (target-conf-string (eyebrowse-config-string target-config)))
    (if target-slot
        (eyebrowse-switch-to-window-config target-slot))
    (cond (buffer
           (eyebrowse-message "select buffer %s at config %s, locked config: %s" buffer target-conf-string locked-conf-string)
           (select-buffer-window-safely buffer))
          (target-slot
           (eyebrowse-message "switch to config %s" target-conf-string)))))

(defun select-buffer-window-safely (buffer &optional name)
  "If BUFFER's window is live, select it, otherwise switch to it or a new buffer named NAME."
  (cond
   (buffer
    (if-let ((window (get-buffer-window buffer)))
	    (select-window window)
	    (switch-to-buffer
	     buffer nil 'force-same-window)))
   (name
    (switch-to-buffer
     name nil 'force-same-window))))

(defun eyebrowse-switch-other-buffer (&rest _)
  "Switch to other buffer, and switch to the window config binding to the buffer first if necessary."
  (interactive)
  (let* ((buffer (other-buffer)))
    (select-buffer-window-safely-at-config buffer)))

(defun eyebrowse-create-window-config-with-tag (tag)
  "Create an eyebrowse window config with TAG."
  (interactive "sTag: ")
  (if (= (length tag) 0)
      (message "tag is empty, can't create window config!")
    (progn
      (eyebrowse-create-window-config)
      (let ((slot (eyebrowse--get 'current-slot)))
        (eyebrowse-rename-window-config slot tag)))))

(defvar *eyebrowse-modify-action-alist*
  '(("set tag" . eyebrowse-rename-window-config)
    ("set as default config" . eyebrowse-set-as-default-config)
    ("restore default config" . eyebrowse-restore-default-config)
    ("create config" . eyebrowse-create-window-config-with-tag)
    ("close config" . eyebrowse-close-window-config)))

(defun eyebrowse-modify-config (&rest _)
  "."
  (interactive)
  (eyebrowse-list-with-actions *eyebrowse-modify-action-alist*))

(defvar *eyebrowse-modify-buffer-action-alist*
  '(("lock buffer's config" . eyebrowse-lock-buffer-config)
    ("free buffer's config" . eyebrowse-free-buffer-config)))

(defun eyebrowse-modify-buffer-config (&rest _)
  "."
  (interactive)
  (eyebrowse-list-with-actions *eyebrowse-modify-buffer-action-alist*))

(defun eyebrowse-list-with-actions (actions)
  "Select one of ACTIONS, and choose candidate from eyebrowse configs."
  (let* ((current-element (eyebrowse-current-config-string))
         (prompt (format "Select eyebrowse action (config %s): " current-element))
         (action (completing-read prompt actions nil t))
         (func (alist-get action actions nil nil #'string=)))
    (when func
      (command-execute func))))

(defun eyebrowse-get-lock-buffer-config (buffer)
  "Get the eyebrowse config that binding to the BUFFER."
  (if buffer
      (with-current-buffer buffer
        (if (boundp '*eyebrowse-locked-config*)
            *eyebrowse-locked-config*
          nil))
    nil))

(defun eyebrowse-lock-with-config (buffer config)
  "Lock the BUFFER that binding to the current eyebrowse CONFIG."
  (with-current-buffer buffer
    (setq-local *eyebrowse-locked-config* config)
    (select-buffer-window-safely-at-config buffer)))

(defun eyebrowse-lock-buffer-config ()
  "Lock the current buffer that binding to the current eyebrowse config."
  (interactive)
  (eyebrowse-list-configs-with-action
   (lambda (element)
     (if (not (-contains? collections element))
         (eyebrowse-message "eyebrowse config %S does not exist!" element)
       (let* ((slot (eyebrowse-config-slot element))
              (config (eyebrowse-get-config-with-slot slot)))
         (eyebrowse-lock-with-config (current-buffer) config))))))

(defun eyebrowse-free-buffer-config ()
  "Free the current buffer that binding to a certain eyebrowse config."
  (interactive)
  (setq-local *eyebrowse-locked-config* nil))

(defun eyebrowse-switch-buffer (&rest _)
  "Switch to another buffer."
  (interactive)
  (let ((buffer (completing-read "Switch to buffer: " #'internal-complete-buffer nil t)))
    ;; preselect: (buffer-name (other-buffer (current-buffer))
    (funcall 'eyebrowse-switch-buffer-action buffer)))

(defun eyebrowse-switch-buffer-action (buffer)
  "Switch to BUFFER may be a string or nil."
  (cond
   ((zerop (length buffer))
    (select-buffer-window-safely nil buffer))
   ((eyebrowse-get-lock-buffer-config buffer)
    (select-buffer-window-safely-at-config buffer))
   (t
    (eyebrowse-message "selected buffer: %S" buffer)
    (let ((configs (eyebrowse--filter-window-config buffer t)))
      (cond
       ((zerop (length configs))
        (select-buffer-window-safely-at-config buffer))
       ((equal 1 (length configs))
        (select-buffer-window-safely-at-config buffer (car configs)))
       (t
        (eyebrowse-list-window-configs configs buffer)))))))

(defun eyebrowse--filter-window-config (target-buffer-name &optional include-current-config)
  "TARGET-BUFFER-NAME, INCLUDE-CURRENT-CONFIG."
  (interactive "bbuffer: ")
  (eyebrowse-update-window-config)
  (let ((filtered-window-config)
        (current-slot (eyebrowse--get 'current-slot)))
    (dolist (window-config (eyebrowse--get 'window-configs))
      (eyebrowse--walk-window-config
       window-config
       (lambda (item)
         (eyebrowse-message "item car: %S" (car item))
         (when (eq (car item) 'buffer)
           (let* ((buffer-name (cadr item))
                  (buffer (get-buffer buffer-name))
                  (matched (string-equal buffer-name target-buffer-name)))
             (cond
              ((not buffer)
               (eyebrowse-message "Replaced deleted %s buffer with *scratch*" buffer-name)
               (setf (cadr item) "*scratch*"))
              (matched
               (eyebrowse-message "matched buffer: %s" (buffer-name buffer))
               (if (not (-contains? filtered-window-config window-config))
                   (push window-config filtered-window-config))))))))
      (when (and (string-prefix-p "*" target-buffer-name)
		 include-current-config
		 (eq current-slot (car window-config))
		 (not (-contains? filtered-window-config window-config)))
        (push window-config filtered-window-config)))
    (reverse filtered-window-config)))

(defun eyebrowse--get-window-config (slot tag &optional window)
  "Copied and modified from eyebrowse--current-window-config.
Returns a window config list appliable for SLOT and TAG of WINDOW."
  (list slot (window-state-get window t) tag))

(defun eyebrowse--update-window-config-element-with-frame (new-element &optional frame)
  "Copied and modified from eyebrowse--update-window-config-element;
with parameter FRAME and NEW-ELEMENT.
Replace the old element with NEW-ELEMENT in the window config list.
The old element is identified by the first element of NEW-ELEMENT."
  (eyebrowse--set 'window-configs
    (--replace-where (= (car it) (car new-element))
                     new-element (eyebrowse--get 'window-configs frame))
    frame))

(defun eyebrowse-update-window-config (&optional frame)
  "Update window config for the FRAME.
If FRAME is nil, update the current frame."
  (let* ((current-slot (eyebrowse--get 'current-slot frame))
	 (window-configs (eyebrowse--get 'window-configs frame))
	 (current-tag (nth 2 (assoc current-slot window-configs))))
    (eyebrowse--update-window-config-element-with-frame
     (eyebrowse--get-window-config current-slot current-tag (frame-root-window frame)) frame)))

(defun eyebrowse-set-as-default-config ()
  "."
  (interactive)
  (eyebrowse-update-window-config)
  (let* ((config (eyebrowse-get-current-config))
         (slot (car config))
         (default-config (assq slot *eyebrowse-default-configs*)))
    (if (not default-config)
        (push config *eyebrowse-default-configs*)
      (setf (cadr (assq slot *eyebrowse-default-configs*)) (cadr config)))))

(defun eyebrowse-restore-default-config ()
  "."
  (interactive)
  (let* ((slot (eyebrowse--get 'current-slot))
         (default-config (assq slot *eyebrowse-default-configs*)))
    (if default-config
        (eyebrowse-load-certain-config default-config)
      (message "no default config to restore config %S" slot))))

(defun eyebrowse-load-certain-config (config)
  "Restore the window config from the certain CONFIG.
COPY from eyebrowse--load-window-config."
  (let ((ignore-window-parameters t)
        (window-config (cadr config)))
    (eyebrowse--fixup-window-config window-config)
    (window-state-put window-config (frame-root-window) 'safe)))

(defun load-eyebrowse-config ()
  "Load eyebrowse workspace from file."
  (interactive)
  (let ((loading-success-p))
    (cond
     ((not (file-exists-p +eyebrowse-file-name+))
      (message "Can't load %s file, for it does not exist!" +eyebrowse-file-name+)
      ;; set loading-success-p to t, for the reason of it's at the very beginnig
      (setq loading-success-p t))
     (t
      (message "Loading eyebrowse config from file %S ..." +eyebrowse-file-name+)
      (with-temp-buffer
        (insert-file-contents +eyebrowse-file-name+)
        (goto-char (point-min))
        (let ((content (read (current-buffer))))
          (eyebrowse--set 'window-configs content)
          (eyebrowse--load-window-config (eyebrowse--get 'current-slot))))
      (setq loading-success-p t)))
    ;; return the loading result
    loading-success-p))

(defun save-eyebrowse-config ()
  "Save eyebrowse workspace to file."
  (interactive)
  (message "Saving eyebrowse config to file %S ..." +eyebrowse-file-name+)
  (eyebrowse-update-window-config)
  (let ((content (format "%S" (eyebrowse--get 'window-configs))))
    (with-temp-file +eyebrowse-file-name+
      (insert content))))

(defun eyebrowse-sync-config ()
  "Sync the eyebrowse config from the first frame."
  (interactive)
  (let ((default-frame (car (last (frame-list))))
        (current-frame (window-frame)))
    (when (not (eq default-frame current-frame))
      (eyebrowse-update-window-config default-frame)
      (when-let ((configs (eyebrowse--get 'window-configs default-frame)))
        (eyebrowse--set 'window-configs configs)
        (eyebrowse--load-window-config (eyebrowse--get 'current-slot))))))

(provide 'eyebrowse+)
;;; eyebrowse+.el ends here
