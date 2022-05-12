;;; list-scratch.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2022 gadmyth

;; Author: list-scratch.el <gadmyth@gmail.com>
;; Version: 1.0.2
;; Package-Version: 20220512.002
;; Package-Requires: json-pointer
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


(defvar *scratch-list* '(("root" . nil)))

(defvar *scratch-current-path* "/root")

(defvar *scratch-current-node* nil)

(defvar *list-scratch-debug* nil)

(defvar *scratch-list-loaded* nil)

(defvar +scratch-list-file-name+ (expand-file-name "~/.scratch_list"))

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
  `((".." . (lambda (data)
              (list-scratch)))
    ("copy" . scratch-copy-node)
    ("x" . nil)
    ))

(defun scratch-copy-node (data)
  "COPY DATA."
  (kill-new data)
  (message "data: %S copied" data))

(defun scratch-node-level-up ()
  "."
  (let ((parent (file-name-directory *scratch-current-path*)))
    (when (> (length parent) 1)
      (setq parent (substring parent 0 (- (length parent) 1))))
    (setq *scratch-current-path* parent))
  (list-scratch-debug-message "level up, current path: %s" *scratch-current-path*)
  (scratch-update-current-node))

(defvar *scratch-list-actions*
  `((".." . (lambda (_ignore)
              (scratch-node-level-up)
              (list-scratch)))
    ("+" . scratch-add-node)
    ("-" . scratch-delete-node)
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
         (when (not (string-equal "/" *scratch-current-path*))
           (setq path (format "%s%s" *scratch-current-path* "/0")))
         (json-pointer-set *scratch-list* path value t :add)
         (scratch-update-current-node)
         (list-scratch)))
      (_
       (let* ((node-type (completing-read "Please select the node type: " '(".." list vector string) nil t nil))
              (key (read-string "Please input key: " nil))
              (value))
         (pcase node-type
           (".." (list-scratch))
           ("list"
            (setq value '()))
           ("vector"
            (setq value []))
           ("string"
            (setq value (read-string "Please input value: " nil))))
         (let ((path (or (and key (format "/%s" key)) "")))
           (when (not (string-equal "/" *scratch-current-path*))
             (setq path (format "%s%s" *scratch-current-path* path)))
           (when (> (length key) 0)
             (json-pointer-set *scratch-list* path value t :set))
           (scratch-update-current-node)
           (list-scratch)))))))

(defun scratch-delete-node (current-node)
  "Delete the sub node under CURRENT-NODE."
  (let* ((node-type (type-of current-node))
         (current-node (or (and (consp current-node) current-node)
                           (and (vectorp current-node) (append current-node nil))
                           current-node))
         (key (completing-read "Please input key to delete: " current-node))
         (path (pcase node-type
                 ('vector
                  (format "/%d" (first-element-index key current-node #'string-equal)))
                 (_
                  (format "/%s" key)))))
    (list-scratch-debug-message "*** %S" current-node)
    (when (not (string-equal "/" *scratch-current-path*))
      (setq path (format "%s%s" *scratch-current-path* path)))
    (list-scratch-debug-message "the path to delete is %s" path)
    (json-pointer-set *scratch-list* path nil t :delete)
    (scratch-update-current-node)
    (list-scratch)))

(defun first-element-index (ele list &optional test-fn)
  "Return first index of ELE in LIST, using TEST-FN to compare."
  (let ((fn (or test-fn #'eq))
        (index))
    (seq-doseq (elem list)
      (if (null index)
          (setq index 0)
        (incf index))
      (when (funcall fn ele elem)
        (return index)))))

(defun list-scratch ()
  "."
  (interactive)
  (cond
   ((or (null *scratch-current-node*) (consp *scratch-current-node*))
    (list-scratch-list))
   ((vectorp *scratch-current-node*)
    (list-scratch-vector))
   (t
    (message "wrong scratch node type: %s" (type-of *scratch-current-node*)))))

(defun list-scratch-list ()
  "."
  (list-scratch-debug-message "current path: %s" *scratch-current-path*)
  (let* ((current-node *scratch-current-node*)
         (path *scratch-current-path*)
         (list current-node)
         ;; add up node
         (list (if (string-equal path "/root") list (cons ".." list)))
         ;; add action node
         (list (seq-concatenate 'list list '("+" "-" "x")))
         (key (completing-read (format "%s: " path) list nil t))
         (value (assoc-default key current-node))
         (list-action))
    (list-scratch-debug-message "key: %S, value: %S" key value)
    (cond
     ((consp value)
      (list-scratch-debug-message "value is list")
      (scratch-level-down key value)
      (list-scratch))
     ((vectorp value)
      (list-scratch-debug-message "value is vector")
      (scratch-level-down key value)
      (list-scratch-vector))
     ((setq list-action (alist-get key *scratch-list-actions* nil nil #'string-equal))
      (list-scratch-debug-message "key is list action")
      (funcall list-action current-node))
     ((not (null value))
      (list-scratch-debug-message "value is string")
      (let* ((action-name (completing-read (format "Node value is %s: " value) *scratch-node-actions*))
             (action (alist-get action-name *scratch-node-actions* nil nil #'string-equal)))
        (when action
          (funcall action value))))
     (t
      (list-scratch-debug-message "value is empty list")
      (scratch-level-down key value)
      (list-scratch)))))

(defun scratch-level-down (path node)
  "Update scratch list down to PATH, and set current-node as NODE."
  (let* ((current-path *scratch-current-path*)
         (seperator (if (string-suffix-p current-path "/") "" "/")))
    (setq *scratch-current-path* (concat current-path seperator path))
    (setq *scratch-current-node* node)))

(defun list-scratch-vector ()
  "."
  (interactive)
  (list-scratch-debug-message "current path: %s" *scratch-current-path*)
  (let* ((current-node *scratch-current-node*)
         (path *scratch-current-path*)
         (list))
    (progn
      (seq-doseq (item current-node) (push item list))
      (setq list (reverse list))
      ;; add up node
      (unless (string-equal path "/") (setq list (cons ".." list)))
      ;; add action node
      (setq list (seq-concatenate 'list list '("+" "-" "x"))))
    ;; select value
    (let ((value (completing-read (format "%s: " path) list)))
      (list-scratch-debug-message "value: %s" value)
      (cond
       ((alist-get value *scratch-list-actions* nil nil #'string-equal)
        (funcall (alist-get value *scratch-list-actions* nil nil #'string-equal) current-node))
       (t
        (when-let ((action-name (completing-read (format "%s %s: " path value) *scratch-node-actions*))
                   (action (alist-get action-name *scratch-node-actions* nil nil #'string-equal)))
          (funcall action value)))))))

(defun save-scratch-list ()
  "Save scratch list to file."
  (interactive)
  (let ((file-name +scratch-list-file-name+))
    (message "Saving scratch list to file %S ..." file-name)
    (let* ((content (replace-regexp-in-string "\\.\\.\\." "" (format "%S" *scratch-list*))))
      (with-temp-file file-name
        (insert content)))))

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

(load-scratch-list)
(add-hook 'kill-emacs-hook #'save-scratch-list)

(provide 'list-scratch)
;;; list-scratch.el ends here
