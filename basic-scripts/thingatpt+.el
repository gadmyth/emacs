;;; thingatpt+.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2022 Gadmyth

;; Author: thingatpt+.el <gadmyth@email.com>
;; Version: 1.0
;; Package-Version: 20221120.001
;; Package-Requires: thingatpt
;; Keywords: thingatpt+.el
;; Homepage: https://www.github.com/gadmyth/emacs
;; URL: https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/thingatpt+.el

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
;; thingatpt+'s code can be found here:
;;   https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/thingatpt+.el

;;; Commentary:
;;; Code:


(require 'thingatpt)

(defmacro toggle-superword-temporarily (toggle-superword-p &rest body)
  "TOGGLE-SUPERWORD-P, BODY."
  `(let* ((origin-value (if (and (boundp 'superword-mode) superword-mode) 1 0))
          (toggle-value (if (and (boundp 'superword-mode) (not superword-mode)) 1 0)))
     (if ,toggle-superword-p (superword-mode toggle-value))
     ,@body
     (if ,toggle-superword-p (superword-mode origin-value))
     (message "should toggle: %S, origin-value: %S, toggle-value: %S"
              ,toggle-superword-p origin-value toggle-value)))


(defun region-or-word-at-point (&optional toggle-superword-p word-bound-p)
  "TOGGLE-SUPERWORD-P, WORD-BOUND-P."
  (let (word)
    (cond
     ((region-active-p)
      (setq word (buffer-substring-no-properties (region-beginning) (region-end)))
      (message "string-at-region is [%s]" word)
      (deactivate-mark))
     (t
      (let* ((toggle-superword-p (or toggle-superword-p
                                     (not (null current-prefix-arg)))))
        (toggle-superword-temporarily
         toggle-superword-p
         (setq word (word-at-point t))
         (when word-bound-p
           (setq word (format "\\<%s\\>" word)))
         (message "string-at-point is [%s]" word)))))
    word))

(defun bound-at-point (&optional toggle-superword-p)
  "TOGGLE-SUPERWORD-P."
  (let (bound)
    (cond
     ((region-active-p)
      (setq bound (car (region-bounds))))
     (t
      (toggle-superword-temporarily
       toggle-superword-p
       (setq bound (bounds-of-thing-at-point 'word)))))
    bound))

(provide 'thingatpt+)
;;; thingatpt+.el ends here
