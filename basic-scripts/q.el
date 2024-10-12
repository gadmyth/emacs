;;; q.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2022 gadmyth

;; Author: q.el <gadmyth@gmail.com>
;; Version: 1.1.1
;; Package-Version: 20241012.001
;; Package-Requires:
;; Keywords: q.el
;; Homepage: https://www.github.com/gadmyth/emacs
;; URL: https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/q.el

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
;; q's code can be found here:
;;   https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/q.el

;;; Commentary:
;;; Code:


(defmacro bound-or-default (var default)
  "If VAR is not bound, return DEFAULT value."
  `(or (and (boundp (quote ,var)) ,var) ,default))

(defmacro set-when-non-null (var value-var)
  "Set VAR as VALUE-VAR when VALUE-VAR is bounded and not null."
  `(when (bound-and-true-p ,value-var)
     (setq ,var ,value-var)))

(defmacro set-with-default (var value-var default)
  "Set VAR as VALUE-VAR when VALUE-VAR is bounded and not null or as DEFAULT value."
  `(cond
    ((bound-and-true-p ,value-var)
     (setq ,var ,value-var))
    (t
     (setq ,var (bound-or-default ,value-var ,default)))))

;; https://curiousprogrammer.wordpress.com/2009/06/08/error-handling-in-emacs-lisp/
(defmacro call-safely (func &rest clean-up)
  "Call FUNC safely, when catch an exception, do CLEAN-UP."
  `(unwind-protect
       (let (retval)
         (condition-case ex
             (setq retval (progn ,func))
           ('error
            (message (format "Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex)))))
         retval)
     ,@clean-up))

(defmacro define-debug-message (module)
  (let ((debug-message-sym (intern (format "%S-debug-message" module)))
        (debug-flag-sym (intern (format "%s-debug-flag" module)))
        (toggle-debug-sym (intern (format "%S-toggle-debug" module))))
    `(progn
       (defvar ,debug-flag-sym nil "debug message flag var")
       
       (defun ,toggle-debug-sym ()
         "."
         (interactive)
         (setq ,debug-flag-sym (not ,debug-flag-sym))
         (message "turn %s the %s" (if ,debug-flag-sym "on" "off") ,debug-flag-sym))
       
       (defmacro ,debug-message-sym (format-string &rest ARGS)
         "If debug is open, send message with FORMAT-STRING and ARGS."
         `(if ,,debug-flag-sym
              (message ,format-string ,@ARGS)))
     )))

(defun y-or-n-p-with-default (prompt &optional default)
  "Ask user a 'y or n' question. Return t if answer is 'y'.
If the user just presses Enter, return DEFAULT if provided, otherwise nil."
  (let ((default (or default 'n)))
    (if (eq (read-char (format "%s(%s): " prompt (if (eq default 'y) "Y/n" "N/y")))
            ?\y)
        t
      (if (eq default 'y)
          t
        nil))))

(provide 'q)
;;; q.el ends here
