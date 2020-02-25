;;; package --- eyebrowse-config.el
;;; Commentary:
;;; Code:

(require 'eyebrowse)
(require 's)
(require 'dash)
(require 'async-config)

(eyebrowse-mode t)
(eyebrowse-rename-window-config 1 "default")

(defvar *eyebrowse-debug* nil)
(defvar +eyebrowse-file-name+ (expand-file-name "~/.eyebrowse_save"))
(defvar *eyebrowse-default-configs* nil)

(add-to-list 'auto-coding-alist '("\\.eyebrowse_save\\'" . utf-8))

(add-hook 'window-setup-hook
          #'(lambda () (call-after 1 (load-eyebrowse-config))))

(add-hook 'kill-emacs-hook 'save-eyebrowse-config)

(defmacro eyebrowse-message (format-string &rest ARGS)
  "If debug is open, send message with FORMAT-STRING and ARGS."
  `(if *eyebrowse-debug*
       (message ,format-string ,@ARGS)))

(defun eyebrowse-get-current-config ()
  "."
  (let* ((slot (eyebrowse--get 'current-slot))
         (config (eyebrowse-get-config-with-slot slot)))
    config))

(defun eyebrowse-get-config-with-slot (slot)
  "SLOT: ."
  (let* ((configs (eyebrowse--get 'window-configs))
         (config (assq slot configs)))
    config))


(defun ivy-eyebrowse-current-config-string ()
  "."
  (let ((current-config (eyebrowse-get-current-config)))
    (ivy-eyebrowse-get-config-string current-config)))

(defun ivy-eyebrowse-get-config-string (config)
  "Get CONFIG string."
  (if (not config)
      nil
    (let* ((slot (car config))
             (tag (nth 2 config)))
        (ivy-eyebrowse-config-string slot tag))))

(defun ivy-eyebrowse-config-string (slot tag)
  "SLOT, TAG."
  (let* ((slot-string (int-to-string slot))
         (ret slot-string))
    (if (> (length tag) 0)
        (setq ret (format "%s: %s" slot-string tag))
      ret)))

(defun ivy-eyebrowse-config-slot (config-string)
  "CONFIG-STRING."
  (if (s-contains? ":" config-string)
      (string-to-number (first (split-string config-string ":")))
    (string-to-number config-string)))

(defun eyebrowse-list-configs ()
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
       (let* ((slot (ivy-eyebrowse-config-slot element))
              (window-configs (eyebrowse--get 'window-configs))
              (config (assq slot window-configs)))
         (select-buffer-window-safely-at-config buffer config))))))

(defun eyebrowse-list-window-configs-with-action (window-configs &optional buffer action)
  "WINDOW-CONFIGS: , BUFFER: , ACTION."
  (let* ((current-config (eyebrowse-get-current-config))
         (current-tag (nth 2 current-config))
         (ivy-prompt (format "Select eyebrowse action (%s): " current-tag))
         (collections))
    (dolist (window-config window-configs)
      (let* ((slot (nth 0 window-config))
             (tag (nth 2 window-config))
             (element (ivy-eyebrowse-config-string slot tag)))
        (eyebrowse-message "window config: %S" element)
        (push element collections)))
    (setf collections (reverse collections))
    (eyebrowse-message "collections: %S" collections)
    (ivy-read ivy-prompt collections
              :preselect current-tag
              :action action)))

(defun select-buffer-window-safely-at-config (buffer &optional config)
  "Switch to window CONFIG, and select BUFFER."
  (let* ((locked-config (eyebrowse-get-lock-buffer-config buffer))
         (locked-slot (car locked-config))
         (target-config (or config locked-config))
         (locked-conf-string (ivy-eyebrowse-get-config-string locked-config))
         (target-slot (car target-config))
         (target-conf-string (ivy-eyebrowse-get-config-string target-config)))
    (if target-slot
        (eyebrowse-switch-to-window-config target-slot))
    (cond (buffer
           (message "select buffer %s at config %s, locked config: %s" buffer target-conf-string locked-conf-string)
           (select-buffer-window-safely buffer))
          (target-slot
           (message "switch to config %s" target-conf-string)))))

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

(defun eyebrowse-switch-other-buffer ()
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

(defun eyebrowse-modify-config ()
  "."
  (interactive)
  (eyebrowse-list-with-actions
   (list (list "set tag" #'eyebrowse-rename-window-config)
         (list "set as default config" #'eyebrowse-set-as-default-config)
         (list "restore default config" #'eyebrowse-restore-default-config)
         (list "create config" #'eyebrowse-create-window-config-with-tag)
         (list "close config" #'eyebrowse-close-window-config))))

(defun eyebrowse-modify-buffer-config ()
  "."
  (interactive)
  (eyebrowse-list-with-actions
   (list
    (list "lock buffer's config" #'eyebrowse-lock-buffer-config)
    (list "free buffer's config" #'eyebrowse-free-buffer-config))))


(defun eyebrowse-list-with-actions (actions)
  "Select one of ACTIONS, and choose candidate from eyebrowse configs."
  (let* ((ivy-actions (mapcar #'car actions))
         (current-element (ivy-eyebrowse-current-config-string))
         (ivy-prompt (format "Select eyebrowse action (config %s): " current-element)))
    (eyebrowse-message "element: %s" current-element)
    (ivy-read ivy-prompt ivy-actions
              :action (lambda (action-string)
                        (let ((action
                               (first (remove-if-not
                                       (lambda (ele)
                                         (equal action-string (first ele)))
                                       actions))))
                          (command-execute (second action)))))))

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
       (let* ((slot (ivy-eyebrowse-config-slot element))
              (config (eyebrowse-get-config-with-slot slot)))
         (eyebrowse-lock-with-config (current-buffer) config))))))

(defun eyebrowse-free-buffer-config ()
  "Free the current buffer that binding to a certain eyebrowse config."
  (interactive)
  (setq-local *eyebrowse-locked-config* nil))

(defun eyebrowse--ivy-switch-buffer ()
  "Switch to another buffer."
  (interactive)
  (ivy-read "Switch to buffer: " #'internal-complete-buffer
            :keymap ivy-switch-buffer-map
            :preselect (buffer-name (other-buffer (current-buffer)))
            :action #'eyebrowse--ivy-switch-buffer-action
            :matcher #'ivy--switch-buffer-matcher
            :caller 'eyebrowse--ivy-switch-buffer))

(defun eyebrowse--ivy-switch-buffer-action (buffer)
  "Switch to BUFFER.
BUFFER may be a string or nil.
Copied from ivy-switch-buffer, and modified."
  (if (zerop (length buffer))
      (select-buffer-window-safely nil ivy-text)
    (let ((virtual (assoc buffer ivy--virtual-buffers))
          (view (assoc buffer ivy-views)))
      (cond ((and virtual
                  (not (get-buffer buffer)))
             (find-file (cdr virtual)))
            (view
             (delete-other-windows)
             (let (
                   ;; silence "Directory has changed on disk"
                   (inhibit-message t))
               (ivy-set-view-recur (cadr view))))
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
                 (eyebrowse-list-window-configs configs buffer)))))))))

(defun eyebrowse--filter-window-config (target-buffer-name &optional include-current-config)
  "TARGET-BUFFER-NAME, INCLUDE-CURRENT-CONFIG."
  (interactive "bbuffer: ")
  (eyebrowse-update-current-window-config)
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

(defun eyebrowse-update-current-window-config ()
  "."
  (let* ((current-slot (eyebrowse--get 'current-slot))
           (window-configs (eyebrowse--get 'window-configs))
           (current-tag (nth 2 (assoc current-slot window-configs))))
    (eyebrowse--update-window-config-element
     (eyebrowse--current-window-config current-slot current-tag))))

(defun eyebrowse-set-as-default-config ()
  "."
  (interactive)
  (eyebrowse-update-current-window-config)
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
  (if (not (file-exists-p +eyebrowse-file-name+))
      (message "Can't load %s file, for it does not exist!" +eyebrowse-file-name+)
    (message "Loading eyebrowse config from file %S ..." +eyebrowse-file-name+)
    (with-temp-buffer
      (insert-file-contents +eyebrowse-file-name+)
      (goto-char (point-min))
      (let ((content (read (current-buffer))))
        (eyebrowse--set 'window-configs content)
        (eyebrowse--load-window-config (eyebrowse--get 'current-slot))))))

(defun save-eyebrowse-config ()
  "Save eyebrowse workspace to file."
  (interactive)
  (message "Saving eyebrowse config to file %S ..." +eyebrowse-file-name+)
  (eyebrowse-update-current-window-config)
  (let ((content (format "%S" (eyebrowse--get 'window-configs))))
    (with-temp-file +eyebrowse-file-name+
      (insert content))))

(provide 'eyebrowse-config)
;;; eyebrowse-config.el ends here
