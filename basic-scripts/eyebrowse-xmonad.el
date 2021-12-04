;;; eyebrowse-xmonad.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2021 gadmyth

;; Author: eyebrowse-xmonad.el <gadmyth@gmail.com>
;; Version: 1.0
;; Package-Version: 20211204.001
;; Package-Requires: eyebrowse, s, windows
;; Keywords: eyebrowse-xmonad
;; Homepage: https://www.github.com/gadmyth/emacs
;; URL: https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/eyebrowse-xmonad.el

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
;; eyebrowse-xmonad's code can be found here:
;;   https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/eyebrowse-xmonad.el

;;; Commentary:
;;; Code:


(require 'eyebrowse)
(require 'windows)
(require 's)

(defvar *eyebrowse-bookmarks* nil)

(defun eyebrowse-add-bookmark (bookmark-key &optional cover-p)
  "Set the current position as eyebrowse bookmark with BOOKMARK-KEY, if COVER-P, overwrite the exist bookmark."
  (interactive "sPlease input the bookmark key to add: ")
  (let* ((buffer (current-buffer))
         (buffername (buffer-name buffer))
         (filename (buffer-file-name buffer))
         (bookmark (alist-get bookmark-key *eyebrowse-bookmarks* nil t 'equal)))
    (when (or (not bookmark) cover-p)
      (setq bookmark `((eyebrowse . ,(eyebrowse--get 'current-slot))
                       (buffername . , buffername)
                       (filename . ,filename)
                       (position . ,(point))))
      (message "%S" bookmark)
      (setf (alist-get bookmark-key *eyebrowse-bookmarks* nil t 'equal) bookmark)
      (define-key eyebrowse-xmonad-mode-map (kbd (format "H-%s" bookmark-key))
        `(lambda ()
           (interactive)
           (eyebrowse-jump-to-bookmark ,bookmark-key))))))

(defun eyebrowse-cover-bookmark (bookmark-key)
  "Cover the exist bookmark of BOOKMARK-KEY."
  (interactive "sPlease input the bookmark key to cover: ")
  (message "cover bookmark: %s" bookmark-key)
  (eyebrowse-add-bookmark bookmark-key t))

(defun eyebrowse-delete-bookmark (bookmark-key)
  "Delete the exist bookmark of BOOKMARK-KEY."
  (interactive "sPlease input the bookmark key to delete: ")
  (message "delete bookmark: %s" bookmark-key)
  (let ((bookmark (alist-get bookmark-key *eyebrowse-bookmarks* nil t 'equal)))
    (setf (alist-get bookmark-key *eyebrowse-bookmarks* nil t 'equal) nil)))

(defun eyebrowse-jump-to-bookmark (bookmark-key)
  "Jump to the eyebrowse bookmark with BOOKMARK-KEY."
  (interactive "sPlease input the bookmark key to jump: ")
  (let ((bookmark (alist-get bookmark-key *eyebrowse-bookmarks* nil t 'equal)))
    (when bookmark
      (let* ((eyebrowse-slot (cdr (assq 'eyebrowse bookmark)))
             (buffername (cdr (assq 'buffername bookmark)))
             (window (get-buffer-window buffername))
             (filename (cdr (assq 'filename bookmark)))
             (position (cdr (assq 'position bookmark))))
        (when (not (eq eyebrowse-slot (eyebrowse--get 'current-slot)))
          (eyebrowse-switch-to-window-config eyebrowse-slot))
        (cond
         (window
          (select-window window))
         ((buffer-live-p (get-buffer buffername))
          (switch-to-buffer buffername))
         ((file-exists-p filename)
          (find-file filename)))
        (goto-char position)))))

(defun define-eyebrowse-bookmarks-keymap ()
  "."
  (let* ((keys "abdefginopqruvwxyz")
         (indice (number-sequence 0 (- (length keys) 1))))
    (seq-doseq (idx indice)
      (let ((key (aref keys idx)))
        (message "%S" key)
        (define-key eyebrowse-xmonad-mode-map (kbd (format "H-M-%c" key))
          `(lambda ()
             (interactive)
             (eyebrowse-add-bookmark ,(format "%c" key))))))
    nil))

(defun eyebrowse-list-bookmarks (&optional action)
  "List the eyebrowse bookmarks, choose one and do ACTION on it."
  (interactive)
  (let* ((prompt "Please select the bookmark: ")
         (collections *eyebrowse-bookmarks*)
         (collections (sort collections (lambda (a b) (string< (car a) (car b)))))
         (collections (mapcar (lambda (bookmark)
                                (let* ((key (car bookmark))
                                       (body (cdr bookmark))
                                       (eyebrowse-slot (cdr (assq 'eyebrowse body)))
                                       (eyebrowse-conf (eyebrowse-get-config-with-slot eyebrowse-slot))
                                       (eyebrowse-conf-string (eyebrowse-config-string eyebrowse-conf))
                                       (buffername (cdr (assq 'buffername body)))
                                       (position (cdr (assq 'position body))))
                                  (format "%s: %s\t%s\t%d" key buffername eyebrowse-conf-string position)))
                              collections))
         (selected (completing-read prompt collections nil t))
         (bookmark-key (substring selected 0 (s-index-of ":" selected))))
    (cond
     (action
      (apply action (list bookmark-key)))
     (t
      (eyebrowse-jump-to-bookmark bookmark-key)))))

(defvar *eyebrowse-bookmark-modify-action-alist*
  '(("cover  bookmark" . (lambda ()
                           (interactive)
                           (eyebrowse-list-bookmarks 'eyebrowse-cover-bookmark)))
    ("delete bookmark" . (lambda ()
                           (interactive)
                           (eyebrowse-list-bookmarks 'eyebrowse-delete-bookmark)))
    ("load   bookmark" . eyebrowse-load-bookmark)
    ("save   bookmark" . eyebrowse-save-bookmark)))

(defun eyebrowse-modify-bookmark ()
  "."
  (interactive)
  (let* ((prompt "Please select the bookmark: ")
         (action (completing-read prompt *eyebrowse-bookmark-modify-action-alist* nil t))
         (func (alist-get action *eyebrowse-bookmark-modify-action-alist* nil nil #'string=)))
    (call-interactively func)))

(defun eyebrowse-switch-to-last-config ()
  "."
  (interactive)
  (when-let ((last-slot (eyebrowse--get 'last-slot)))
    (eyebrowse-switch-to-window-config last-slot)))

(defmacro define-eyebrowse-extend-switch-config-function (slot)
  "SLOT."
  `(defun ,(intern (format "eyebrowse-switch-to-window-config-%d-with-tag" slot)) ()
     (interactive)
     (if (not (eyebrowse--window-config-present-p ,slot))
         (let ((tag (read-string (format "Set tag for the new eyebrowse config of slot %d: " ,slot))))
           (eyebrowse-switch-to-window-config ,slot)
           (eyebrowse-rename-window-config ,slot tag))
       (eyebrowse-switch-to-window-config ,slot))))

(defvar eyebrowse-xmonad-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "H-`") #'eyebrowse-switch-to-last-config)
    (define-key map (kbd "H-1") (define-eyebrowse-extend-switch-config-function 1))
    (define-key map (kbd "H-2") (define-eyebrowse-extend-switch-config-function 2))
    (define-key map (kbd "H-3") (define-eyebrowse-extend-switch-config-function 3))
    (define-key map (kbd "H-4") (define-eyebrowse-extend-switch-config-function 4))
    (define-key map (kbd "H-5") (define-eyebrowse-extend-switch-config-function 5))
    (define-key map (kbd "H-6") (define-eyebrowse-extend-switch-config-function 6))
    (define-key map (kbd "H-7") (define-eyebrowse-extend-switch-config-function 7))
    (define-key map (kbd "H-8") (define-eyebrowse-extend-switch-config-function 8))
    (define-key map (kbd "H-9") (define-eyebrowse-extend-switch-config-function 9))
    (define-key map (kbd "H-t") #'run-or-raise-next-terminal)
    (define-key map (kbd "H-m") #'goto-main-window)
    (define-key map (kbd "<H-tab>") #'goto-next-window)
    (define-key map (kbd "<H-iso-lefttab>") #'goto-previous-window)
    (define-key map (kbd "<H-return>") #'swap-to-main-window)
    (define-key map (kbd "<H-backspace>") #'goto-last-window)
    (define-key map (kbd "H-s") #'swap-window-in-current-frame)
    (define-key map (kbd "H-c") #'copy-window-in-current-frame)
    (define-key map (kbd "H-h") #'adjust-window-size)
    (define-key map (kbd "H-l") #'adjust-window-size)
    (define-key map (kbd "H-j") #'adjust-window-size)
    (define-key map (kbd "H-k") #'adjust-window-size)
    map)
  "Initial key map for `eyebrowse-xmonad-mode'.")

(define-minor-mode eyebrowse-xmonad-mode
  "Toggle `eyebrowse-xmonad-mode."
  :keymap eyebrowse-xmonad-mode-map
  :global t
  (when eyebrowse-xmonad-mode
    (define-eyebrowse-bookmarks-keymap)))

(provide 'eyebrowse-xmonad)
;;; eyebrowse-xmonad.el ends here
