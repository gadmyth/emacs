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
  (let* ((window-configs (eyebrowse--get 'window-configs))
         (slot 1)
         (current-slot (eyebrowse--get 'current-slot))
         (current-tag (nth 2 (assoc current-slot window-configs)))
         (current-element (ivy-eyebrowse-config-string current-slot current-tag))
         (next-slot (1+ current-slot))
         (next-slot (if (> next-slot (- (length window-configs) 1)) 0 next-slot))
         (next-tag (nth 2 (assoc next-slot window-configs)))
         (next-element (ivy-eyebrowse-config-string next-slot next-tag))
         (ivy-prompt (format "Select eyebrowse action (%s): " current-element))
         (collections '()))
    (dolist (window-config window-configs)
      (let* ((tag (nth 2 window-config))
             (element (ivy-eyebrowse-config-string slot tag)))
        (setq collections (push element collections))
        (incf slot)))
    (message "collections: %S" collections)
    (ivy-read ivy-prompt (reverse collections)
              :preselect current-element
              :action (lambda (element)
                        (let ((slot (ivy-eyebrowse-config-slot element)))
                          (eyebrowse-switch-to-window-config slot))))))

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

(provide 'eyebrowse-config)
;;; eyebrowse-config.el ends here