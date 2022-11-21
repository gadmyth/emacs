;;; clipboard+.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2022 Gadmyth

;; Author: clipboard+.el <gadmyth@gmail.com>
;; Version: 1.0
;; Package-Version: 20221121.001
;; Package-Requires:
;; Keywords: clipboard
;; Homepage: https://www.github.com/gadmyth/emacs
;; URL: https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/clipboard+.el

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
;; clipboard+'s code can be found here:
;;   https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/clipboard+.el

;;; Commentary:
;;; Code:


(defun try-kill-to-system-clipboad (content)
  "CONTENT."
  (cond
   ((> (length content) 0)
    (cond
     ((executable-find "xclip")
      (let* ((mktemp-cmd (executable-find "mktemp"))
             (tmp-file-path (shell-command-to-string (format "%s /tmp/emacs.king-ring.XXXX" mktemp-cmd)))
             (tmp-file-path (replace-regexp-in-string "\n" "" tmp-file-path))
             (xclip-cmd (executable-find "xclip"))
             (command (format "cat %s | xclip -sel c" tmp-file-path xclip-cmd)))
        (message "content: %s" content)
        (message "temp file: %s" tmp-file-path)
        (message "command: %s" command)
        (write-region content nil tmp-file-path)
        (call-process-shell-command command)
        (delete-file tmp-file-path)))
     (t
      (kill-new content))))
   (t
    (message "Can't copy empty content!"))))

(provide 'clipboard+)
;;; clipboard+.el ends here
