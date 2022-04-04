;;; ffap+.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2022 gadmyth

;; Author: ffap+.el <gadmyth:@gmail.com>
;; Version: 1.0
;; Package-Version: 20220404.001
;; Package-Requires: ffap
;; Keywords: ffap
;; Homepage: https://www.github.com/gadmyth/emacs
;; URL: https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/ffap+.el

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
;; ffap+'s code can be found here:
;;   https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/ffap+.el

;;; Commentary:
;;; Code:

(require 'ffap)

(defun ffap-maybe-sudo ()
  "."
  (interactive)
  (let ((path (ffap-string-at-point)))
    (when (> (length path) 0)
      (when (y-or-n-p "Prepend sudo or not?")
        (setq path (format "/sudo::%s" (expand-file-name path))))
      (ffap path))))

(provide 'ffap+)
;;; ffap+.el ends here
