;;; password-generator+.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2021 gadmyth

;; Author: password-generator+.el <gadmyth@gmail.com>
;; Version: 1.1.0
;; Package-Version: 20250604.001
;; Package-Requires: password-generator
;; Keywords: password-generator
;; Homepage: https://www.github.com/gadmyth/emacs
;; URL: https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/password-generator+.el

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
;; password-generator+'s code can be found here:
;;   https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/password-generator+.el

;;; Commentary:
;;; Code:

(require 'password-generator)

(defun swap (list index1 index2)
  "In LIST swap indices INDEX1 and INDEX2 in place."
  (let ((tmp (elt list index1)))
    (setf (elt list index1) (elt list index2))
    (setf (elt list index2) tmp)))

(defun shuffle (list)
  "Shuffle the elements in LIST."
  (loop for i in (reverse (number-sequence 1 (1- (length list))))
        do (let ((j (random (+ i 1))))
             (when (not (eq i j))
               (swap list i j))))
  list)

(defun password-generator-generate ()
  "."
  (interactive)
  (password-generator-normal nil nil nil nil nil t))
  
(defun password-generator-normal (&optional total-len number-len lowercase-len uppercase-len special-len return)
  "."
  (interactive)
  (let* ((password "")
         (number-len (or number-len 2))
         (uppercase-len (or uppercase-len 2))
         (special-len (or special-len 2))
         (total-len (or total-len current-prefix-arg))
         (lowercase-len (if total-len (- total-len number-len uppercase-len special-len)
                          (or lowercase-len 10))))
    (when (and (> number-len 0)
               (> uppercase-len 0)
               (> special-len 0)
               (> lowercase-len 0))
      (let* ((number-part (password-generator-generate-internal "0123456789" number-len))
             (uppercase-part (password-generator-generate-internal "ABCDEFGHIJKLMNOPQRSTUVWXYZ" uppercase-len))
             (special-part (password-generator-generate-internal "!@#$%^&*()_-+=/?,.><[]{}~" special-len))
             (lowercase-part (password-generator-generate-internal "abcdefghijklmnopqrstuvwxyz" lowercase-len))
             (password (concat number-part uppercase-part special-part lowercase-part))
             (password (shuffle password)))
        (cond ((equal nil return) (insert password)) (t password))))))


(defun password-generator-simple (&optional pre-len return)
  "Minimal viable password for most of web systems.  It is not secure but allow to register.  PRE-LEN is prefix arg that defines password lenght.  RETURN specifies if password should be returned or inserted."
  (interactive)
  (let* (
         (password "")
         (pass-length (or pre-len current-prefix-arg password-generator-simple-length))
         (symbols-for-pass "abcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
    (setq password (password-generator-generate-internal symbols-for-pass pass-length))
    (cond ((equal nil return) (insert password)) (t password))))

(defun generate-random-java-class-name ()
  "."
  (interactive)
  (let ((head (password-generator-get-random-string-char "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
        (other (password-generator-simple (+ 10 (random 11)) t)))
    (concat head other)))

(provide 'password-generator+)
;;; password-generator+.el ends here
