;;; xref+.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2024 Gadmyth

;; Author: xref+.el <gadmyth@gmail.com>
;; Version: 1.0.2
;; Package-Version: 20240822.001
;; Package-Requires: xref
;; Keywords: xref, counsel, grep
;; Homepage: https://www.github.com/gadmyth/emacs
;; URL: https://www.github.com/gadmyth/emacs/blob/master/basic-script/xref+.el

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
;; xref+'s code can be found here:
;;   https://www.github.com/gadmyth/emacs/blob/master/basic-script/xref+.el

;;; Commentary:
;;; Code:


(require 'files+)

(defvar *xref-action-list*
  (cl-remove-if
   #'null
   `(
     ,(when (featurep 'counsel)
        `("counsel-git" . counsel-git-with-word-at-point))
     ,(when (featurep 'counsel)
        `("counsel-git-grep" . counsel-git-grep-with-word-at-point))
     ("find project file" . find-project-file-at-point)
     ("goto class method" . java-jump-to-class-method)
     ("goto class property" . java-jump-to-class-properties)
     )))

(defun xref-custom-action ()
  "This function is called when xref-find-definitions finds no results."
  (message "No definitions found. Executing custom function.")
  (let* ((default (caar *xref-action-list*))
         (action (completing-read "Choose xref custom action:" *xref-action-list* nil t nil nil default))
         (f (assoc-default action *xref-action-list*)))
    (funcall f)))

(defun xref-find-definitions-advice (origin-fun &rest args)
  "Advice around `xref-find-definitions' to call `my-custom-function' when no results are found."
  (condition-case err
    (apply origin-fun args)
    (t
     (message "Caught an error: %s" err)
    (xref-custom-action))))

(advice-add 'xref-find-definitions :around #'xref-find-definitions-advice)
(remove-hook 'xref-backend-functions #'etags--xref-backend)

(provide 'xref+)
;;; xref+.el ends here
