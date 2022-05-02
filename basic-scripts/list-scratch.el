;;; list-scratch.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2022 gadmyth

;; Author: list-scratch.el <gadmyth@gmail.com>
;; Version: 1.0
;; Package-Version: 20220416.001
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


(defvar *scratch-list*
  '(("root" . (("a" . "1") ("b" . (("d" . ["6" "8" "9"]))) ("c" . "3")))))

(defvar *scratch-current-node* nil)

(defvar *scratch-current-path* "/root")

(defun scratch-update-current-node ()
  "."
  (setq *scratch-current-node* (json-pointer-get *scratch-list* *scratch-current-path* t)))

(defvar *scratch-node-actions*
  `((".." . (lambda (data)
              (list-scratch)))
    ("yank" . scratch-yank-node)
    ("x" . nil)
    ))

(defun scratch-yank-node (data)
  "Yank DATA."
  (message "data: %S" data))

(defun scratch-node-level-up ()
  "."
  (let ((parent (file-name-directory *scratch-current-path*)))
    (when (> (length parent) 1)
      (setq parent (substring parent 0 (- (length parent) 1))))
    (setq *scratch-current-path* parent))
  (message "level up, current path: %s" *scratch-current-path*)
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
  (let* ((key (read-string "Please input key: " nil))
         (value (read-string "Please input value: " nil))
         (path (format "/%s" key)))

    (when (not (string-equal "/" *scratch-current-path*))
      (setq path (format "%s%s" *scratch-current-path* path)))

    (when (and (> (length key) 0)
               (> (length value) 0))
      (json-pointer-set *scratch-list* path value t)
      (scratch-update-current-node)
      (list-scratch))))

(defun scratch-delete-node (current-node)
  "Delete the sub node under CURRENT-NODE."
  (let* ((key (read-string "Please input key to delete: " nil))
         (path (format "/%s" key)))
    (when (not (string-equal "/" *scratch-current-path*))
      (setq path (format "%s%s" *scratch-current-path* path)))
    (message "the path to delete is %s" path)
    (json-pointer-set *scratch-list* path nil t)
    (scratch-update-current-node)
    (list-scratch)))

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
  (message "current path: %s" *scratch-current-path*)
  (let* ((current-node *scratch-current-node*)
         (path *scratch-current-path*)
         (list current-node)
         ;; add up node
         (list (if (string-equal path "/root") list (cons ".." list)))
         ;; add action node
         (list (seq-concatenate 'list list '("+" "-" "x")))
         (key (completing-read (format "%s: " path) list nil t))
         (value (assoc-default key current-node)))
    (cond
     ;; is cons
     ((consp value)
      (setq *scratch-current-path* (concat path (if (string-suffix-p path "/") "" "/") key))
      (setq *scratch-current-node* value)
      (list-scratch))
     ;; is vector
     ((vectorp value)
      (setq *scratch-current-path* (concat path (if (string-suffix-p path "/") "" "/") key))
      (setq *scratch-current-node* value)
      (list-scratch-vector)
      )
     ((alist-get key *scratch-list-actions* nil nil #'string-equal)
      (funcall (alist-get key *scratch-list-actions* nil nil #'string-equal) current-node))
     (t
      (let* ((action-name (completing-read (format "%s: " value) *scratch-node-actions*))
             (action (alist-get action-name *scratch-node-actions* nil nil #'string-equal)))
        (when action
          (funcall action value)))))))
  (interactive)
  (message "current path: %s" *scratch-current-path*)
  (let* ((current-node *scratch-current-node*)
         (path *scratch-current-path*)
         (list))
    (progn
      (seq-doseq (item current-node) (push item list))
      (setq list (reverse list))
      ;; add up node
      (unless (string-equal path "/") (setq list (cons ".." list)))
      ;; add action node
      (setq list (seq-concatenate 'list list '("+" "x"))))
    ;; select value
    (let ((value (completing-read (format "%s: " path) list)))
      (message "value: %s" value)
      (cond
       ((alist-get value *scratch-list-actions* nil nil #'string-equal)
        (funcall (alist-get value *scratch-list-actions* nil nil #'string-equal) current-node))
       (t
        (when-let ((action-name (completing-read (format "%s: " value) *scratch-node-actions*))
                   (action (alist-get action-name *scratch-node-actions* nil nil #'string-equal)))
          (funcall action value)))))))

(provide 'list-scratch)
;;; list-scratch.el ends here
