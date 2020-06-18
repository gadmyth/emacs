;;; ox+.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2020 gadmyth

;; Author: ox+.el <gadmyth@gmail.com}>
;; Version: 0.01
;; Package-Version: 20200619.001
;; Package-Requires: org
;; Keywords: ox+.el
;; Homepage: https://www.github.com/gadmyth/emacs
;; URL: https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/ox+.el

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
;; ox+'s code can be found here:
;;   https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/ox+.el

;;; Commentary:
;;; Code:

(require 'ox-md)

(defun org-md-example-block (example-block _contents info)
  "Transcode EXAMPLE-BLOCK element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let* ((block (cadr example-block))
         (language (plist-get block :language))
         (value (plist-get block :value))
         (new-value (format "```%s\n%s\n```" language value)))
    (plist-put block :value new-value))
  (org-remove-indentation
   (org-export-format-code-default example-block info)))

(provide 'ox+)
;;; ox+.el ends here
