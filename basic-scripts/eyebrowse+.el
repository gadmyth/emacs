;;; eyebrowse+.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2020 gadmyth

;; Author: eyebrowse+.el <gadmyth@gmail.com>
;; Version: 1.2.13
;; Package-Version: 20211206.001
;; Package-Requires: eyebrowse, s, dash, network-util, weathers
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
(require 'network-util)
(require 'weathers)

(defvar *eyebrowse-debug* nil)
(defvar +eyebrowse-file-name+ (expand-file-name "~/.eyebrowse_save"))
(defvar *eyebrowse-default-configs* nil)
(defvar eyebrowse-lazy-load-hook nil)
(defvar *eyebrowse-init-function-swapped* nil)

(defface eyebrowse-time-face
  '((((class color) (background light)) (:foreground "sea green" :weight bold))
    (((class color) (background dark)) (:foreground "SteelBlue3" :weight bold)))
  "Face for eyebrowse time."
  :group 'eyebrowse+)

(defface current-eyebrowse-config-face
  '((((class color) (background light)) (:foreground "forest green" :weight bold))
    (((class color) (background dark)) (:foreground "forest green" :weight bold)))
  "Face for current eyebrowse config text."
  :group 'eyebrowse+)

(defface last-eyebrowse-config-face
  '((((class color) (background light)) (:foreground "dark slate gray" :weight bold))
    (((class color) (background dark)) (:foreground "dark slate gray" :weight bold)))
  "Face for last eyebrowse config text."
  :group 'eyebrowse+)

(add-to-list 'auto-coding-alist '("\\.eyebrowse_save\\.*\\'" . utf-8))

(defun eyebrowse-init-original (&optional frame)
  "Original function of eyebrowse-init with option parameter FRAME.")

(defun eyebrowse-init-from-config (&optional frame)
  "Initialize Eyebrowse for the current FRAME."
  (message "*** eyebrowse-init-from-config ***")
  (unless (eyebrowse--get 'window-configs frame)
    (eyebrowse-lazy-load-config)))

(defun eyebrowse-swap-init-function()
  "."
  (unless *eyebrowse-init-function-swapped*
    (let ((func (symbol-function 'eyebrowse-init)))
      (setf (symbol-function 'eyebrowse-init-original) func)
      (setf (symbol-function 'eyebrowse-init) #'eyebrowse-init-from-config))
    (setq *eyebrowse-init-function-swapped* t)))

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

(defun eyebrowse-list-configs-with-action (action &optional buffer)
  "ACTION: , BUFFER."
  (interactive)
  (eyebrowse-list-window-configs-with-action (eyebrowse--get 'window-configs) buffer action))

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
         (last-config (eyebrowse-get-last-config))
         (current-slot (nth 0 current-config))
         (current-tag (nth 2 current-config))
         (last-slot (nth 0 last-config))
         (default-slot (nth 0 (or last-config current-config)))
         (default-candidate)
         (prompt "Select eyebrowse action: ")
         (collections))
    (dolist (window-config window-configs)
      (let* ((slot (nth 0 window-config))
             (tag (nth 2 window-config))
             (element (eyebrowse-config-string window-config)))
        (eyebrowse-message "window config: %S" element)
        (cond ((= current-slot slot)
               (setq element (propertize element 'face 'current-eyebrowse-config-face)))
              ((= last-slot slot)
               (setq element (propertize element 'face 'last-eyebrowse-config-face))))
        (when (= default-slot slot)
          (setq default-candidate (eyebrowse-config-string (eyebrowse-get-config-with-slot default-slot))))
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

    ;; if has target slot, switch to config
    (when target-slot
      (eyebrowse-switch-to-window-config target-slot)
      (eyebrowse-message "switch to config %s" target-conf-string))
    

    ;; if buffer is not locked to an config, show the actions to just switch buffer or lock config
    (if (and
         (null target-slot)
         (null (eyebrowse-get-lock-buffer-config buffer))
         (not (string-prefix-p "*" (string-trim (or (and (stringp buffer) buffer) (buffer-name buffer)))))
         (buffer-file-name (get-buffer buffer)))
        (funcall #'eyebrowse-switch-buffer-with-actions buffer)
      (progn
        (eyebrowse-message "select buffer %s at config %s, locked config: %s" buffer target-conf-string locked-conf-string)
        (select-buffer-window-safely buffer)))))

(defun select-buffer-window-safely (buffer &optional name)
  "If BUFFER's window is live, select it, otherwise switch to it or a new buffer named NAME."
  (cond
   (buffer
    (if-let ((window (get-buffer-window buffer)))
        (select-window window)
      (switch-to-buffer buffer nil 'force-same-window)))
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
    ("close config" . eyebrowse-close-window-config)
    ("load config" . load-eyebrowse-config)
    ("save config" . save-eyebrowse-config)))

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

(defvar *eyebrowse-switch-buffer-action-alist*
  '(("switch to the buffer" . select-buffer-window-safely)
    ("lock buffer's config" . eyebrowse-lock-buffer-config)))

(defun eyebrowse-switch-buffer-with-actions (&rest args)
  "ARGS."
  (interactive)
  (eyebrowse-message "actions args: %S" args)
  (apply #'eyebrowse-list-with-actions *eyebrowse-switch-buffer-action-alist* args))

(defun eyebrowse-list-with-actions (actions &rest args)
  "Select one of ACTIONS, and choose action from eyebrowse configs, call the action with ARGS."
  (let* ((current-config (eyebrowse-current-config-string))
         (prompt (format "Select eyebrowse action (%s): " current-config))
         (action (completing-read prompt actions nil t))
         (func (alist-get action actions nil nil #'string=)))
    (when func
      (eyebrowse-message "list actions, actions: %s, args: %s" actions args)
      (if args
          (apply func args)
        (call-interactively func)))))

(defun eyebrowse-get-lock-buffer-config (buffer)
  "Get the eyebrowse config that binding to the BUFFER."
  (if buffer
      (with-current-buffer buffer
        (if (and (boundp '*eyebrowse-locked-config-slot*) *eyebrowse-locked-config-slot*)
            (eyebrowse-get-config-with-slot *eyebrowse-locked-config-slot*)
          nil))
    nil))

(defun eyebrowse-lock-with-config (buffer config)
  "Lock the BUFFER that binding to the current eyebrowse CONFIG."
  (with-current-buffer buffer
    (setq-local *eyebrowse-locked-config-slot* (car config))
    (select-buffer-window-safely-at-config buffer)))

(defun eyebrowse-lock-buffer-config (&optional buffer)
  "Lock the current BUFFER that binding to the current eyebrowse config."
  (interactive)
  (eyebrowse-list-configs-with-action
   (lambda (element)
     (if (not (-contains? collections element))
         (eyebrowse-message "eyebrowse config %S does not exist!" element)
       (let* ((slot (eyebrowse-config-slot element))
              (config (eyebrowse-get-config-with-slot slot)))
         (eyebrowse-message "locked, buffer: %s" buffer)
         (eyebrowse-lock-with-config (or buffer (current-buffer)) config))))
   buffer))

(defun eyebrowse-free-buffer-config (&optional buffer)
  "Free the current BUFFER that binding to a certain eyebrowse config."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (setq-local *eyebrowse-locked-config-slot* nil)))

(defun eyebrowse-buffer-name-with-config (buffer)
  "BUFFER."
  (let* ((name (buffer-name buffer))
         (locked-config (eyebrowse-get-lock-buffer-config buffer))
         (config-string (eyebrowse-config-string locked-config))
         (info (if (null config-string) "" (format " (%s)" config-string)))
         (name-with-config (format "%s%s" name info)))
    name-with-config))

(defun temporary-buffer-p (buffer-name)
  "Check the buffer of BUFFER-NAME is temporary or not."
  (or (string-prefix-p " " buffer-name) (string-prefix-p "*" buffer-name)))

(defun eyebrowse-switch-buffer (&rest _)
  "Switch to another buffer."
  (interactive)
  (let* ((current-buffer (current-buffer))
         (current-buffer-name (eyebrowse-buffer-name-with-config (current-buffer)))
         (last-buffer (other-buffer (current-buffer) t))
         (last-buffer-name (eyebrowse-buffer-name-with-config last-buffer))
         (locked-config (eyebrowse-get-lock-buffer-config current-buffer))
         (config-string (eyebrowse-config-string locked-config))
         (info (if (null config-string) "" (format " (%s)" config-string)))
         (buffer-list (mapcar #'eyebrowse-buffer-name-with-config (buffer-list)))
         (buffer-list (remove-if #'temporary-buffer-p buffer-list))
         (current (car buffer-list)))
    ;; propertize the current buffer
    (when (equal current-buffer-name current)
      (setq current (propertize current 'face 'current-eyebrowse-config-face))
      (setf (car buffer-list) current))
    ;; show the buffer list
    (let ((buffer (completing-read (format "Switch to buffer%s: " info) buffer-list nil t nil nil last-buffer-name)))
      ;;  (buffer (completing-read (format "Switch to buffer%s: " info) #'internal-complete-buffer nil t)))
      (funcall 'eyebrowse-switch-buffer-action-with-config buffer))))

(defun eyebrowse-switch-buffer-action-with-config (buffer-name-with-config)
  "Switch to buffer may be a string or nil, BUFFER-NAME-WITH-CONFIG."
  (let ((buffer (replace-regexp-in-string " (.*)" "" buffer-name-with-config)))
    (eyebrowse-switch-buffer-action buffer)))

(defun eyebrowse-switch-buffer-action (buffer-name)
  "Switch to buffer of BUFFER-NAME may be a string or nil."
  (cond
   ((zerop (length buffer))
    (select-buffer-window-safely nil buffer))
   ((eyebrowse-get-lock-buffer-config buffer)
    (select-buffer-window-safely-at-config buffer))
   (t
    (eyebrowse-message "selected buffer: %S" buffer)
    (let ((configs (eyebrowse--filter-window-config buffer t)))
      (eyebrowse-message "filted window config: %S" (length configs))
      (cond
       ((zerop (length configs))
        (select-buffer-window-safely-at-config buffer))
       ((equal 1 (length configs))
        (select-buffer-window-safely-at-config buffer (car configs)))
       (t
        (eyebrowse-list-window-configs configs buffer)))))))

(defun eyebrowse--filter-window-config (target-buffer-name &optional include-current-config)
  "TARGET-BUFFER-NAME, INCLUDE-CURRENT-CONFIG, list all configs that show the buffer."
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

(defun eyebrowse-config-the-only-config ()
  "."
  (interactive)
  (let* ((window-configs (eyebrowse--get 'window-configs))
         (first-config (car window-configs))
         (tag (nth 2 first-config)))
    (when (and (= (length window-configs) 1)
               (zerop (length tag)))
      (eyebrowse-message "rename first eyebrowse config as \"default\"")
      (eyebrowse-rename-window-config 1 "default")
      (force-reset-eyebrowse-header-line-format))))

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
  (let ((loading-success-p)
        (file-name (eyebrowse-file-name)))
    (eyebrowse-init-original)
    (cond
     ((not (file-exists-p file-name))
      (message "Can't load %s file, for it does not exist!" file-name)
      ;; set loading-success-p to t, for the reason of it's at the very beginnig
      (setq loading-success-p t))
     (t
      (message "Loading eyebrowse config from file %S ..." file-name)
      (with-temp-buffer
        (insert-file-contents file-name)
        (goto-char (point-min))
        (let* ((rich-configs (read (current-buffer)))
               (eyebrowse-configs (cdr (assq 'eyebrowse rich-configs)))
               (frame-params (cdr (assq 'frame-params rich-configs))))
          (when (> (length eyebrowse-configs) 0)
            (eyebrowse--set 'window-configs eyebrowse-configs)
            (eyebrowse--load-window-config (eyebrowse--get 'current-slot))
            (modify-frame-parameters (window-frame) frame-params))))
      (setq loading-success-p t)))
    ;; set the only config's tag
    (eyebrowse-config-the-only-config)
    ;; return the loading result
    loading-success-p))

(defun frame-index ()
  "."
  (let* ((frames (frame-list))
         (current-frame (window-frame (get-buffer-window)))
         (max-index (- (length frames) 1))
         (index (- max-index (-elem-index current-frame frames))))
    index))

(defun frame-index-string ()
  "Use number unicode for frame index from https://unicode-table.com."
  (let ((frame-index (frame-parameter nil 'eyebrowse-frame-index)))
    (cond ((eq frame-index 0) "➊")
          ((eq frame-index 1) "➋")
          ((eq frame-index 2) "➌")
          ((eq frame-index 3) "➍")
          ((eq frame-index 4) "➎")
          ((eq frame-index 5) "➏")
          ((eq frame-index 6) "➐")
          ((eq frame-index 7) "➑")
          ((eq frame-index 8) "➒")
          ((eq frame-index 9) "❿")
          (t ""))))


(defun eyebrowse-file-name (&optional frame)
  "Get the eyebrowse file name of FRAME."
  (interactive)
  (let ((frame-index (frame-parameter frame 'eyebrowse-frame-index)))
    (unless frame-index
      (setq frame-index (frame-index))
      (set-frame-parameter nil 'eyebrowse-frame-index frame-index))
    (if frame-index
        (format "%s.%d" +eyebrowse-file-name+ frame-index)
      +eyebrowse-file-name+)))

(defun eyebrowse-frame-parameters (&optional frame)
  "Get the FRAME parameters to save or load."
  (let ((top (frame-parameter frame 'top))
        (left (frame-parameter frame 'left))
        (width (frame-parameter frame 'width))
        (height (frame-parameter frame 'height)))
    `((top . ,top) (left . ,left) (width . ,width) (height . ,height))))

(defun save-eyebrowse-config (&optional frame)
  "Save eyebrowse workspace to file of FRAME."
  (interactive)
  (let ((file-name (eyebrowse-file-name frame)))
    (message "Saving eyebrowse config to file %S ..." file-name)
    (eyebrowse-update-window-config)
    (let* ((eyebrowse-configs (eyebrowse--get 'window-configs))
           (frame-params (eyebrowse-frame-parameters))
           (rich-configs `((frame-params . ,frame-params) (eyebrowse . ,eyebrowse-configs))))
      (with-temp-file file-name
        (print rich-configs (current-buffer))))))

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

(defun eyebrowse-make-keymap (slot)
  "SLOT."
  (let ((map (make-sparse-keymap)))
    ;; the mode-line event
    ;(define-key map (kbd "<mode-line><mouse-1>")
    (define-key map (kbd "<header-line><mouse-1>")
      `(lambda (_e)
         (interactive "e")
         (eyebrowse-switch-to-window-config ,slot)))
    map))

(defvar eyebrowse-buffer-name-format
  '(:eval
    (let* ((buffer (current-buffer))
           (buffer-name (buffer-name buffer))
           (locked-conf (and (boundp '*eyebrowse-locked-config-slot*)
                             (eyebrowse-get-config-with-slot *eyebrowse-locked-config-slot*)))
           (locked-conf-string (eyebrowse-config-string locked-conf))
           (current-conf (eyebrowse-get-current-config))
           (current-conf-string (eyebrowse-config-string current-conf))
           (last-conf (eyebrowse-get-last-config))
           (last-conf-string (eyebrowse-config-string last-conf))
           (help-echo "mouse-1: Switch to indicated workspace"))
      ;; show file name first, if nil show buffer name; and also show the buffer-locked and current eyebrowse config
      (list
       " "
       (let ((index (frame-index-string)))
       (if index (list index " ") ""))
       (let* ((time-string (format-time-string "%Y-%m-%d %H:%M %a" (current-time)))
              (time-string-list (s-split " " time-string))
              (date-string (car time-string-list))
              (time-string (propertize (cadr time-string-list) 'face 'eyebrowse-time-face))
              (week-string (caddr time-string-list)))
         (list date-string " " time-string " " week-string))
       " | "
       (current-ip)
       (if (> (length *fetched-public-ip*) 0) " | " "")
       (fetched-public-ip)
       " | "
       (fetched-weather)
       " | "
       ;; copy the default buffer identification from bindings.el.gz
       (propertized-buffer-identification "%b")
       " | "
       ;; - [locked-conf, current-conf, last-conf]
       (format "[%s, %s, %s]"
               locked-conf-string
               ;; the current eb config is active, and with no keymap
               (propertize current-conf-string 'face 'current-eyebrowse-config-face
                           'mouse-face 'mode-line-highlight
                           'slot (car current-conf)
                           'local-map nil
                           'help-echo help-echo)
               ;; last-conf can be clicked to the last eb config
               (propertize last-conf-string 'face 'last-eyebrowse-config-face
                           'mouse-face 'mode-line-highlight
                           'slot (car last-conf)
                           'local-map (eyebrowse-make-keymap (car last-conf))
                           'help-echo help-echo)
               )))))

(defvar eyebrowse-config-format
  '(:eval
    (list
     " "
     (let ((index (frame-index-string)))
       (if index (list index " ") ""))
     "["
     (-interpose
      ", "
      (let ((current-slot (eyebrowse--get 'current-slot))
            (last-slot (eyebrowse--get 'last-slot))
            (help-echo "mouse-1: Switch to indicated workspace"))
        (mapcar (lambda (config)
                  (let* ((slot (car config))
                         (config-string (eyebrowse-config-string config))
                         (config-string (if (and (boundp '*eyebrowse-locked-config-slot*)
                                                 (eq slot *eyebrowse-locked-config-slot*))
                                            (format "%s*" config-string)
                                          config-string))
                         (face)
                         (local-map))
                    (cond ((eq slot current-slot)
                           (setq face 'current-eyebrowse-config-face))
                          ((eq slot last-slot)
                           (setq face 'last-eyebrowse-config-face)
                           (setq local-map (eyebrowse-make-keymap (car config))))
                          (t
                           (setq local-map (eyebrowse-make-keymap (car config)))))
                    (propertize config-string 'face face
                                'mouse-face 'mode-line-highlight
                                'slot slot
                                'local-map local-map
                                'help-echo help-echo)))
                (eyebrowse--get 'window-configs))))
     "]")))

(defmacro walk-all-frame-windows (&rest body)
  "Walk all windows in all frame for each execute the BODY."
  `(walk-windows (lambda (window)
                   (with-current-buffer (window-buffer window)
                     ,@body))
                 nil t))

(defun set-eyebrowse-header-line-format ()
  "."
  (interactive)
  (when (or (not header-line-format)
            current-prefix-arg)
    (when (window-live-p (get-buffer-window))
      (with-current-buffer (current-buffer)
        (eyebrowse-message "set-eyebrowse-header-line-format, buffer: %s" (current-buffer))
        (setq-local header-line-format eyebrowse-config-format)))))

(defun reset-eyebrowse-header-line-format ()
  "."
  (interactive)
  (when (not (equal header-line-format eyebrowse-config-format))
    (setq-local header-line-format eyebrowse-config-format)))

(defun force-reset-eyebrowse-header-line-format ()
  "."
  (interactive)
  (setq-local header-line-format eyebrowse-config-format))

(defun switch-header-line-format ()
  "."
  (interactive)
  (with-current-buffer (current-buffer)
    (if (equal header-line-format eyebrowse-buffer-name-format)
        (setq-local header-line-format eyebrowse-config-format)
      (setq-local header-line-format eyebrowse-buffer-name-format))))

(defvar eyebrowse-plus-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "H-~") 'switch-header-line-format)
    map)
  "Initial key map for `eyebrowse-plus-mode'.")

(define-minor-mode eyebrowse-plus-mode
  "Toggle `eyebrowse-plus-mode."
  :keymap eyebrowse-plus-mode-map
  :global t
  (if eyebrowse-plus-mode
      (progn
        (eyebrowse-swap-init-function)
        (add-hook 'eyebrowse-lazy-load-hook
                  (lambda ()
                    (add-hook 'delete-frame-functions #'save-eyebrowse-config)
                    (add-hook 'kill-emacs-hook #'save-eyebrowse-config)
                    (set-eyebrowse-header-line-format)))
        ;; set mode-line-buffer-identification
        ;(setq-default mode-line-buffer-identification eyebrowse-buffer-name-format)
        (add-hook 'find-file-hook #'set-eyebrowse-header-line-format)
        (add-hook 'window-configuration-change-hook #'set-eyebrowse-header-line-format)
        (add-hook 'eyebrowse-post-window-switch-hook #'reset-eyebrowse-header-line-format)
        (eyebrowse-mode 1))
    (progn
      (remove-hook 'delete-frame-functions #'save-eyebrowse-config)
      (remove-hook 'kill-emacs-hook #'save-eyebrowse-config)
      ;; header-line-format hook
      (remove-hook 'find-file-hook #'set-eyebrowse-header-line-format)
      (remove-hook 'window-configuration-change-hook #'set-eyebrowse-header-line-format)
      (remove-hook 'eyebrowse-post-window-switch-hook #'reset-eyebrowse-header-line-format)
      (walk-all-frame-windows
       (setq-local header-line-format nil))
      (eyebrowse-mode 0))))

(provide 'eyebrowse+)
;;; eyebrowse+.el ends here
