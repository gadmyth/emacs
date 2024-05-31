;;; minibuffer+.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2021 gadmyth

;; Author: minibuffer+.el <gadmyth@gmail.com>
;; Version: 1.0.1
;; Package-Version: 20240531.001
;; Package-Requires: minibuffer
;; Keywords: minibuffer+
;; Homepage: https://www.github.com/gadmyth/emacs
;; URL: https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/minibuffer+.el

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
;; minibuffer+'s code can be found here:
;;   https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/minibuffer+.el

;;; Commentary:
;;; Code:


(require 'minibuffer)
(require 'delsel)

(defvar *minibuffer-func* nil)

(defun minibuffer-func-p (func)
  "Check the *minibuffer-func* is FUNC."
  (equal *minibuffer-func* func))

(defun call-and-mark-func (func &rest args)
  "Call FUNC interactively with ARGS."
  (setq *minibuffer-func* func)
  (call-interactively func args))

(defun quit-minibuffer-and-unmark-func ()
  "Quit minibuffer and unmark *minibuffer-func*."
  (minibuffer-keyboard-quit)
  (setq *minibuffer-func* nil))

(defmacro toggle-minibuffer (func)
  "When in a minibuffer, exit the minibuffer, or call the FUNC."
  "ATTENION: this can't pass the args."
  `(lambda (&rest args)
     (interactive)
     (if (window-minibuffer-p)
         (if (minibuffer-func-p ,func)
             (quit-minibuffer-and-unmark-func)
           (progn
             (run-with-timer
              0 nil
              (lambda (f &rest args) (call-and-mark-func f args)) ,func args))
           (minibuffer-keyboard-quit))
       (call-and-mark-func ,func args))))

(provide 'minibuffer+)
;;; minibuffer+.el ends here
