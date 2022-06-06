;;; q.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2022 gadmyth

;; Author: q.el <gadmyth@gmail.com>
;; Version: 1.0
;; Package-Version: 20220511.001
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

(provide 'q)
;;; q.el ends here
