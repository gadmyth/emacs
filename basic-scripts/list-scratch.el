;;; list-scratch.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2022 gadmyth

;; Author: list-scratch.el <gadmyth@gmail.com>
;; Version: 1.0.7
;; Package-Version: 20220528.001
;; Package-Requires: json-pointer, dash, dates
;; Keywords: list-scratch.el
;; Homepage: https://www.github.com/gadmyth/emacs
;; URL: https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/list-scratch.el

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
;; list-scratch's code can be found here:
;;   https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/list-scratch.el

;;; Commentary:
;;; Code:


(require 'json-pointer)
(require 'dash)
(require 'dates)

(defvar *scratch-list* '(("root" . nil)))

(defvar *scratch-current-path* "/root")

(defvar *scratch-current-node* nil)

(defvar *list-scratch-debug* nil)

(defvar *scratch-list-loaded* nil)

(defvar +scratch-list-file-name+ (expand-file-name "~/.scratch_list"))

(defvar *scratch-list-modified* nil)

(defvar *scratch-list-idle-save-timer* nil)

(defvar *scratch-list-idle-delay* 300)


(defun list-scratch-toggle-debug ()
  "."
  (interactive)
  (setq *list-scratch-debug* (not *list-scratch-debug*))
  (message "turn %s the *list-scratch-debug*" (if *list-scratch-debug* "on" "off")))

(defmacro list-scratch-debug-message (format-string &rest ARGS)
  "If debug is open, send message with FORMAT-STRING and ARGS."
  `(if *list-scratch-debug*
       (message ,format-string ,@ARGS)))

(defun scratch-update-current-node ()
  "."
  (setq *scratch-current-node* (json-pointer-get *scratch-list* *scratch-current-path* t)))

(defun scratch-reload-list ()
  "."
  (interactive)
  (setq *scratch-current-path* "/root")
  (scratch-update-current-node))

(defun scratch-level-up-to-root ()
  "."
  (interactive)
  (setq *scratch-current-path* "/root")
  (scratch-update-current-node))

(defvar *scratch-node-actions*
  `((".." . (lambda (key value)
              (scratch-node-level-up)
              (list-scratch)))
    ("copy" . scratch-copy-node)
    ("edit in buffer" . scratch-edit-in-buffer)
    ("open link" . scratch-open-link)
    ("~" . scratch-rename-leaf)
    ("x" . nil)
    ))

(defun scratch-copy-node (key value)
  "COPY VALUE of KEY."
  (kill-new value)
  (message "data: %S copied" value))

(defun scratch-edit-in-buffer (key value)
  "Edit VALUE of KEY in a buffer."
  (let ((buffer (get-buffer-create "*scratch-edit-buffer*")))
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      (erase-buffer)
      (scratch-edit-mode t)
      (insert value))))

(defun scratch-finish-edit-in-buffer ()
  "."
  (interactive)
  ;; save json value
  (with-current-buffer "*scratch-edit-buffer*"
    (let* ((value (buffer-string))
           (path *scratch-current-path*))
      (when (> (length value) 0)
        (json-pointer-set *scratch-list* path value t :set t)
        (setq *scratch-list-modified* t))))
  ;; kill buffer
  (kill-buffer "*scratch-edit-buffer*")
  ;; update json node
  (scratch-update-current-node)
  (list-scratch))

(defun scratch-open-link (key value)
  "Open link of KEY's VALUE."
  (when value
    (org-open-link-from-string value)))

(defun scratch-parent-path (path)
  "Get the PATH's parent path."
  (let ((parent (file-name-directory path)))
    (when (> (length parent) 1)
      (setq parent (substring parent 0 (- (length parent) 1))))
    parent))

(defun scratch-node-level-up ()
  "."
  (setq *scratch-current-path* (scratch-parent-path *scratch-current-path*))
  (list-scratch-debug-message "level up, current path: %s" *scratch-current-path*)
  (scratch-update-current-node))

(defvar *scratch-list-actions*
  `((".." . (lambda (_ignore)
              (scratch-node-level-up)
              (list-scratch)))
    ("+" . scratch-add-node)
    ("-" . scratch-delete-node)
    ("~" . scratch-rename-node)
    ("x" . (lambda (_ignore)
             nil))
    ))

(defun scratch-add-node (current-node)
  "Add a new node to scratch CURRENT-NODE."
  (let ((type (type-of current-node)))
    (pcase type
      ('vector
       (let ((path)
             (value (read-string "Please input value: " nil)))
         (when (not (string= "/" *scratch-current-path*))
           (setq path (format "%s%s" *scratch-current-path* "/[0]")))
         (json-pointer-set *scratch-list* path value t :add)
         (setq *scratch-list-modified* t)
         (scratch-update-current-node)
         (list-scratch)))
      (_
       (let* ((node-type (completing-read "Please select the node type: " '(".." string list vector org-capture) nil t nil))
              (key)
              (value))
         (pcase node-type
           (".."
            (list-scratch))
           ("string"
            (scratch-add-string-type-node current-node))
           ("list"
            (scratch-add-list-type-node current-node))
           ("vector"
            (scratch-add-vector-type-node current-node))
           ("org-capture"
            (scratch-add-org-capture-node current-node))))))))

(defun scratch-add-string-type-node (current-node)
  "Add string type node under CURRENT-NODE."
  (let* ((key (completing-read "Please input key to add as string: " (cons ".." current-node)))
         (default (json-pointer-get current-node (scratch-concat-path "/" key) t))
         (value))
    (cond
     ((string= key "..")
      (list-scratch))
     (t
      (setq value (read-string (format "Please input value for %s: " key) default))
      (scratch-do-add-node key value)))))

(defun scratch-add-list-type-node (current-node)
  "Add list type node under CURRENT-NODE."
  (let* ((key (completing-read "Please input key to add as list: " (cons ".." current-node)))
         (value))
    (cond
     ((string= key "..")
      (list-scratch))
     (t
      (setq value '())
      (scratch-do-add-node key value)))))

(defun scratch-add-vector-type-node (current-node)
  "Add vector type node under CURRENT-NODE."
  (let* ((key (completing-read "Please input key to add as vector: " (cons ".." current-node)))
         (value))
    (cond
     ((string= key "..")
      (list-scratch))
     (t
      (setq value [])
      (scratch-do-add-node key value)))))

(defun scratch-add-org-capture-node (current-node)
  "Add org capture node under CURRENT-NODE."
  (when *org-cap-temp*
    (let ((key (assoc-default "description" *org-cap-temp* #'string=))
          (value (assoc-default "link" *org-cap-temp* #'string=)))
      (scratch-do-add-node key value)
      (setq *org-cap-temp* nil))))

(defun scratch-do-add-node (key value)
  "The common logic of add VALUE of KEY."
  (when (> (length key) 0)
    (let ((path (scratch-concat-path *scratch-current-path* key)))
      (json-pointer-set *scratch-list* path value t :set)
      (setq *scratch-list-modified* t))
    (scratch-update-current-node)
    (list-scratch)))

(defun scratch-delete-node (current-node)
  "Delete the sub node under CURRENT-NODE."
  (let* ((node-type (type-of current-node))
         (current-node (or (and (consp current-node) current-node)
                           (and (vectorp current-node) (append current-node nil))
                           current-node))
         (key (completing-read "Please input key to delete: " (cons ".." current-node)))
         (path))
    (cond
     ((string= key "..")
      (list-scratch))
     (t
      (setq path (pcase node-type
                   ('vector
                    (format "/[%d]" (first-element-index key current-node #'string=)))
                   (_
                    (format "/%s" key))))
      (list-scratch-debug-message "*** %S" current-node)
      (when (not (string= "/" *scratch-current-path*))
        (setq path (format "%s%s" *scratch-current-path* path)))
      (list-scratch-debug-message "the path to delete is %s" path)
      (json-pointer-set *scratch-list* path nil t :delete)
      (setq *scratch-list-modified* t)
      (scratch-update-current-node)
      (list-scratch)))))

(defun scratch-rename-leaf (key value)
  "Rename CURRENT-NODE's KEY with VALUE."
  (let ((new-key (read-string "Please input a new key name: "))
        (path *scratch-current-path*))
    (message "current-path: %s, new key name is: %s" *scratch-current-path* new-key)
    (json-pointer-set *scratch-list* path new-key t :rename)
    (setq *scratch-list-modified* t)
    (scratch-node-level-up)
    (scratch-level-down new-key)
    (list-scratch)))

(defun scratch-rename-node (current-node)
  "Rename CURRENT-NODE's key name."
  (let ((new-key (read-string "Please input a new key name: "))
        (path *scratch-current-path*))
    (message "current-path: %s, new key name is: %s" *scratch-current-path* new-key)
    (json-pointer-set *scratch-list* path new-key t :rename)
    (setq *scratch-list-modified* t)
    (scratch-node-level-up)
    (scratch-level-down new-key)
    (list-scratch)))

(defun first-element-index (ele list &optional test-fn)
  "Return first index of ELE in LIST, using TEST-FN to compare."
  (let ((fn (or test-fn #'eq))
        (index)
        (result))
    (seq-doseq (elem list)
      (if (null index)
          (setq index 0)
        (incf index))
      (when (funcall fn ele elem)
        (setq result index)))
    result))

(defun list-scratch ()
  "."
  (interactive)
  (cond
   ((or (null *scratch-current-node*) (consp *scratch-current-node*))
    (list-scratch-debug-message "list-scratch-list")
    (list-scratch-list))
   ((vectorp *scratch-current-node*)
    (list-scratch-debug-message "list-scratch-vector")
    (list-scratch-vector))
   (t
    (list-scratch-debug-message "list-scratch-string")
    (list-scratch-string))))

(defun list-scratch-list ()
  "."
  (list-scratch-debug-message "current path: %s" *scratch-current-path*)
  (let* ((current-node *scratch-current-node*)
         (node-type (cond ((vectorp current-node) "#")
                          ((consp current-node) "*")
                          ((null current-node) "*")))
         (path *scratch-current-path*)
         (list current-node)
         ;; add up node
         (list (if (string= path "/root") list (cons ".." list)))
         ;; add other action node
         (action-list (or (and (string= path "/root")
                               '("+" "-" "x"))
                          '("+" "-" "~" "x")))
         (list (seq-concatenate 'list list action-list))
         (key (completing-read (format "%s %s: " node-type path) list nil t))
         (value (assoc-default key current-node))
         (list-action))
    (list-scratch-debug-message "key: %S, value: %S" key value)
    (cond
     ((consp value)
      (list-scratch-debug-message "value is list")
      (scratch-level-down-with-node key value)
      (list-scratch))
     ((vectorp value)
      (list-scratch-debug-message "value is vector")
      (scratch-level-down-with-node key value)
      (list-scratch-vector))
     ((setq list-action (assoc-default key *scratch-list-actions* #'string=))
      (list-scratch-debug-message "key is list action")
      (funcall list-action current-node))
     ((not (null value))
      (list-scratch-debug-message "value is string")
      (scratch-level-down-with-node key value)
      (list-scratch-string-with-param path key value))
     (t
      (list-scratch-debug-message "value is empty list")
      (scratch-level-down-with-node key value)
      (list-scratch)))))

(defun list-scratch-string ()
  "."
  (let* ((path (scratch-parent-path *scratch-current-path*))
         (key (file-name-nondirectory *scratch-current-path*))
         (value *scratch-current-node*))
    (list-scratch-string-with-param path key value)))

(defun list-scratch-string-with-param (path key value)
  "."
  (let* ((list *scratch-node-actions*)
         (list (-insert-at 1 value list))
         (action-name (completing-read (format "%s's value: " (scratch-concat-path path key)) list))
         (action (assoc-default action-name *scratch-node-actions* #'string=)))
    (when action
      (funcall action key value))))

(defun scratch-concat-path (parent path)
  "Concat PARENT with PATH."
  (let ((seperator (if (string-suffix-p parent "/") "" "/"))
        (path (if (string-prefix-p path "/") (substring path 1) path)))
    (concat parent seperator path)))

(defun scratch-level-down-with-node (path node)
  "Update scratch list down to PATH, and set current-node as NODE."
  (let* ((current-path *scratch-current-path*))
    (setq *scratch-current-path* (scratch-concat-path current-path path))
    (setq *scratch-current-node* node)))

(defun scratch-level-down (path)
  "Update scratch list down to PATH, and set current-node as NODE."
  (let* ((current-path *scratch-current-path*)
         (new-path (scratch-concat-path current-path path))
         (node (json-pointer-get *scratch-list* new-path t)))
    (setq *scratch-current-path* new-path)
    (setq *scratch-current-node* node)))

(defun list-scratch-vector ()
  "."
  (list-scratch-debug-message "current path: %s" *scratch-current-path*)
  (let* ((current-node *scratch-current-node*)
         (node-type (cond ((vectorp current-node) "#")
                          ((consp current-node) "*")
                          ((null current-node) "*")))
         (path *scratch-current-path*)
         (list))
    (progn
      (seq-doseq (item current-node) (push item list))
      (setq list (reverse list))
      ;; add up node
      (unless (string= path "/") (setq list (cons ".." list)))
      ;; add action node
      (setq list (seq-concatenate 'list list '("+" "-" "x"))))
    ;; select value
    (let ((value (completing-read (format "%s %s: " node-type path) list)))
      (list-scratch-debug-message "value: %s" value)
      (cond
       ;; value is action
       ((assoc-default value *scratch-list-actions* #'string=)
        (funcall (assoc-default value *scratch-list-actions* #'string=) current-node))
       (t
        ;; value is normal node
        (let ((index-path (format "[%d]" (first-element-index value current-node #'string=))))
          (scratch-level-down index-path)
          (list-scratch)))))))

(defvar scratch-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x #") 'scratch-finish-edit-in-buffer)
    map)
  "Initial key map for `scratch-edit-mode'.")

(define-minor-mode scratch-edit-mode
  "Toggle `scratch-edit-mode."
  :keymap scratch-edit-mode-map
  :global nil
  )

(defun save-scratch-list ()
  "Save scratch list to file."
  (interactive)
  (let ((file-name +scratch-list-file-name+))
    (message "Saving scratch list to file %S ..." file-name)
    (let* ((content (replace-regexp-in-string "\\.\\.\\." "" (format "%S" *scratch-list*))))
      (with-temp-file file-name
        (insert content)))
    (setq *scratch-list-modified* nil)))

(defun load-scratch-list ()
  "Load scratch list from file."
  (interactive)
  (unless *scratch-list-loaded*
    (let ((file-name +scratch-list-file-name+))
      (cond
       ((not (file-exists-p file-name))
        (message "Can't load %s file, for it does not exist!" file-name)
        (setq *scratch-list-loaded* t))
       (t
        (message "Loading scratch list from file %S ..." file-name)
        (with-temp-buffer
          (insert-file-contents file-name)
          (goto-char (point-min))
          (setq *scratch-list* (read (current-buffer)))
          (scratch-reload-list)
          (setq *scratch-list-loaded* t)))))))

(defun save-scratch-list-when-modified ()
  "."
  (message "### %s Now saving the scratch list..." (current-time-normal-string))
  (let* ((now (current-timestamp)))
    (cond
     (*scratch-list-modified*
      (message "### scratch list is modified, now saving..." (current-time-normal-string))
      (save-scratch-list))
     (t
      (message "### scratch list is not modified, ignore." (current-time-normal-string))))))

(defun start-scratch-list-idle-saver ()
  "Start a idle saver to save scratch list."
  (unless *scratch-list-idle-save-timer*
    (setq *scratch-list-idle-save-timer*
          (run-with-idle-timer
           *scratch-list-idle-delay*
           *scratch-list-idle-delay*
           #'save-scratch-list-when-modified))))

(load-scratch-list)
(start-scratch-list-idle-saver)
(add-hook 'kill-emacs-hook #'save-scratch-list)

(provide 'list-scratch)
;;; list-scratch.el ends here
