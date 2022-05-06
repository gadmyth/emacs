;;; json-pointer.el --- JSON pointer implementation in Emacs Lisp

;; Copyright (C) 2022 by gadmyth
;; Copyright (C) 2017 by Syohei YOSHIDA

;; Author: Gadmyth <gadmyth@gmail.com>
;; URL: https://github.com/gadmyth/emacs/basic-scripts/json-pointer.el
;; Version: 1.0.1

;; Copied and modified from following repository:
;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-json-pointer
;; Version: 0.01

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; JSON pointer implementation in Emacs Lisp

;;; Code:

(require 'cl-lib)

(defvar *json-pointer-debug* nil)

(defun json-pointer-toggle-debug ()
  "."
  (interactive)
  (setq *json-pointer-debug* (not *json-pointer-debug*))
  (message "turn %s the *json-pointer-debug*" (if *json-pointer-debug* "on" "off")))

(defmacro json-pointer-debug-message (format-string &rest ARGS)
  "If debug is open, send message with FORMAT-STRING and ARGS."
  `(if *json-pointer-debug*
       (message ,format-string ,@ARGS)))

(defun json-pointer--parse-path (path &optional path-string-p)
  "Parse json PATH, if PATH-STRING-P is t, consider path is a string, not a symbol."
  (let ((paths (split-string path "/" t)))
    (cl-loop for path in paths
             for p1 = (replace-regexp-in-string "~1" "/" path)
             for p2 = (replace-regexp-in-string "~0" "~" path)
             collect
             (cond ((string-match-p "^[[:digit:]]*$" p2)
                    (string-to-number p2))
                   (path-string-p
                    p2)
                   (t
                    (intern p2)))
             )))

;;;###autoload
(defun json-pointer-get (json path &optional path-string-p)
  "Get value from JSON of PATH, if PATH-STRING-P is t, path is not a symbol."
  (let ((data json)
        (paths (json-pointer--parse-path path path-string-p)))
    (cl-loop for p in paths
             if (and (consp data) (assoc p data))
             do
             (setq data (assoc-default p data))
             else

             if (and (vectorp data) (integerp p) (> (length data) p))
             do
             (setq data (aref data p))
             else
             return nil

             finally return data)))

;;;###autoload
(defun json-pointer-set (json path value &optional path-string-p)
  "Set VALUE in JSON of PATH, if PATH-STRING-P is t, path is not a symbol."
  (let* ((data json)
         (paths (json-pointer--parse-path path path-string-p))
         (len (length paths))
         (parent)
         (parent-path))
    (json-pointer-debug-message "the whole path is %s [%S], value is %s" path paths value)
    (cl-loop for p in paths
             for i in (number-sequence 1 len)
             do
             (json-pointer-debug-message "----")
             (json-pointer-debug-message "path: %s\ntype: %s\ndata: %S\ntype: %s\nparent: %S"
                                         p (type-of p)
                                         data (type-of data)
                                         parent)
             (json-pointer-debug-message "----")
             (cond
              ;; is last path
              ((= i len)
               ;; is cons
               (json-pointer-debug-message "*** is last path ***")
               (cond
                ((or (null data) (consp data))
                 (json-pointer-debug-message "test: %S" (assoc p data))
                 (let ((find (assoc p data)))
                   (when (or (not find)
                             (y-or-n-p
                              (cond (value
                                     (format "Override the value %S ?" (assoc-default p data)))
                                    (t
                                     (format "Delete the value %s ?" p)))))
                     (json-pointer-debug-message "now do the set (%s, %s) into %S..." p value data)
                     (when (and path-string-p (symbolp p))
                       (setq p (symbol-name p)))
                     (when (and path-string-p (symbolp parent-path))
                       (setq parent-path (symbol-name parent-path)))
                     (when parent
                       (cond
                        (value
                         (assoc-set-depth-2 parent parent-path p value))
                        (t
                         (assoc-delete-depth-2 parent parent-path p)))
                       (json-pointer-debug-message "after set, data is %S"
                                (assoc-get-depth-2 parent parent-path p))))))
                ;; is vector
                ((and (vectorp data) (integerp p) (> (length data) p))
                 (setf (aref data p) value))
                (t
                 (json-pointer-debug-message "data: %S, type: %S" data (type-of data))
                 (message "set json value: wrong node type, is neither cons nor vector"))
                ))
              ;; is cons
              ((and (consp data) (assoc p data))
               (setq parent data)
               (setq parent-path p)
               (setq data (assoc-default p data)))
              ;; is vector
              ((and (vectorp data) (integerp p) (> (length data) p))
               (setq parent data)
               (setq parent-path p)
               (setq data (aref data p)))
              (t
               (message "find json node, wrong node type, is neither cons nor vector")
               (cl-return))))))

(defun assoc-set-depth-2 (data path1 path2 value)
  "Set /PATH1/PATH2 of DATA as VALUE."
  (setf (alist-get path2
                   (alist-get path1 data nil nil #'string-equal)
                   nil nil #'string-equal)
        value))

(defun assoc-delete-depth-2 (data path1 path2)
  "Delete /PATH1/PATH2 of DATA."
  (setf (alist-get path2
                   (alist-get path1 data nil nil #'string-equal)
                   :remove :remove #'string-equal)
        :remove))

(defun assoc-get-depth-2 (data path1 path2)
  "Get value of /PATH1/PATH2 of DATA."
  (alist-get path2
             (alist-get path1 data nil nil #'string-equal)
             nil nil #'string-equal))

(provide 'json-pointer)

;;; json-pointer.el ends here