;;; package --- eyebrowse-config.el
;;; Commentary:
;;; Code:

(require 'eyebrowse)
(require 's)

(eyebrowse-mode t)

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

(defun eyebrowse-list-window-configs (window-configs &optional buffer)
  "WINDOW-CONFIGS: , BUFFER: ."
  (let* ((current-config (assq (eyebrowse--get 'current-slot) window-configs))
         (current-tag (nth 2 current-config))
         (ivy-prompt (format "Select eyebrowse action (%s): " current-tag))
         (collections))
    (dolist (window-config window-configs)
      (let* ((slot (nth 0 window-config))
             (tag (nth 2 window-config))
             (element (ivy-eyebrowse-config-string slot tag)))
        (message "window config: %S" element)
        (push element collections)))
    (setf collections (reverse collections))
    (message "collections: %S" collections)
    (ivy-read ivy-prompt collections
              :preselect current-tag
              :action (lambda (element)
                        (let ((slot (ivy-eyebrowse-config-slot element)))
                          (eyebrowse-switch-to-window-config slot)
                          (if buffer
                              (select-window (get-buffer-window buffer))))))))

(defun eyebrowse-create-window-config-with-tag (tag)
  "TAG."
  (interactive "sTag: ")
  (eyebrowse-create-window-config)
  (if (> (length tag) 0)
      (let ((current-slot (eyebrowse--get 'current-slot)))
        (eyebrowse-rename-window-config current-slot tag))))

(defun eyebrowse-list-actions ()
  "."
  (interactive)
  (let* ((actions (list (list "set tag" #'eyebrowse-rename-window-config)
                        (list "create config" #'eyebrowse-create-window-config-with-tag)
                        (list "close config" #'eyebrowse-close-window-config)))
         (ivy-actions (mapcar #'car actions))
         (window-configs (eyebrowse--get 'window-configs))
         (current-slot (eyebrowse--get 'current-slot))
         (current-tag (nth 2 (assoc current-slot window-configs)))
         (current-element (ivy-eyebrowse-config-string current-slot current-tag))
         (ivy-prompt (format "Select eyebrowse action (config %s): " current-element)))
    (message "element: %s, current-slot: %S, current-tag: %S" current-element current-slot current-tag)
    (ivy-read ivy-prompt ivy-actions
              :action (lambda (action-string)
                        (let ((action (first (remove-if-not
                                              (lambda (ele)
                                                (equal action-string (first ele))) actions))))
                          (command-execute (second action)))))))


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
BUFFER may be a string or nil."
  (if (zerop (length buffer))
      (switch-to-buffer
       ivy-text nil 'force-same-window)
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
            (t
             (message "selected buffer: %S" buffer)
             (let ((configs (eyebrowse--filter-window-config buffer)))
               (cond
                ((zerop (length configs))
                 (switch-to-buffer
                  buffer nil 'force-same-window))
                ((equal 1 (length configs))
                 (let* ((config (car configs))
                        (slot (nth 0 config)))
                   (eyebrowse-switch-to-window-config slot)
                   (select-window (get-buffer-window buffer))))
                (t
                 (eyebrowse-list-window-configs configs buffer)))))))))

(defun eyebrowse--filter-window-config (target-buffer-name)
  "TARGET-BUFFER-NAME: ."
  (interactive "bbuffer: ")
  (eyebrowse-update-current-window-config)
  (let ((filtered-window-config))
    (dolist (window-config (eyebrowse--get 'window-configs))
      (eyebrowse--walk-window-config
       window-config
       (lambda (item)
         (let ((debug nil))
           (if debug (message "item car: %S" (car item))))
         (when (eq (car item) 'buffer)
           (let* ((buffer-name (cadr item))
                  (buffer (get-buffer buffer-name))
                  (matched (string-equal buffer-name target-buffer-name)))
             (if (not buffer)
                 (progn
                   (message "Replaced deleted %s buffer with *scratch*" buffer-name)
                   (setf (cadr item) "*scratch*"))
               (when matched
                 (message "matched buffer: %s" (buffer-name buffer))
                 (if (not (-contains? filtered-window-config window-config))
                     (push window-config filtered-window-config)))))))))
    (reverse filtered-window-config)))

(defun eyebrowse-update-current-window-config ()
  "."
  (let* ((current-slot (eyebrowse--get 'current-slot))
           (window-configs (eyebrowse--get 'window-configs))
           (current-tag (nth 2 (assoc current-slot window-configs))))
    (eyebrowse--update-window-config-element
     (eyebrowse--current-window-config current-slot current-tag))))

(provide 'eyebrowse-config)
;;; eyebrowse-config.el ends here