;;; browse-url+.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2022 Gadmyth

;; Author: browse-url+.el <gadmyth@gmail.com>
;; Version: 1.0.1
;; Package-Version: 20220612.001
;; Package-Requires: w3m
;; Keywords: browse url
;; Homepage: https://www.github.com/gadmyth/emacs
;; URL: https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/browse-url+.el

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
;; browse-url+'s code can be found here:
;;   https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/browse-url+.el

;;; Commentary:
;;; Code:

(defvar *browse-url-function-list*
  `((browse-url-default-browser . browse-url)
    (eww-browse-url . browse-url)
    ,(if (featurep 'w3m-load)
         `(w3m-browse-url . browse-url))
    (browse-url-firefox . browse-url)
    (browse-url-chrome . browse-url)
    (show-link-url . show-link-url)))

(defun browse-url-select-function (url)
  "Select function from list to open the URL."
  (let* ((action-list *browse-url-function-list*)
         (action (intern (completing-read "Select the browser: "
                                          action-list nil t)))
         (func (alist-get action action-list nil nil #'string=))
         (browse-url-browser-function action))
    (message "browser function is: %s" browse-url-browser-function)
    (if (equal func #'browse-url)
        (if (not (fboundp browse-url-browser-function))
            (message "%s is not implemented!" browse-url-browser-function)
          (browse-url url))
      (funcall func url))))

(defun show-link-url (url)
  "Show link's URL."
  (message "link url is: %s" url))

(provide 'browse-url+)
;;; browse-url+.el ends here
