;;; eyebrowse-xmonad.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2021 gadmyth

;; Author: eyebrowse-xmonad.el <gadmyth@gmail.com>
;; Version: 1.0.7
;; Package-Version: 20221224.001
;; Package-Requires: eyebrowse, s, windows, minibuffer+
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
(require 'minibuffer+)
(require 's)

(defvar *eyebrowse-bookmarks* nil)

(defvar *eyebrowse-current-bookmark-key* nil)

(defvar *eyebrowse-last-bookmark-key* nil)

(defconst +eyebrowse-bookmarks-file-name+ (expand-file-name "~/.eyebrowse_bookmarks"))

(defvar eyebrowse-bookmark-search-size 16)

(defun eyebrowse-add-bookmark (bookmark-key &optional cover-p)
  "Set the current position as eyebrowse bookmark with BOOKMARK-KEY, if COVER-P, overwrite the exist bookmark."
  (interactive "sPlease input the bookmark key to add: ")
  (let* ((buffer (current-buffer))
         (buffername (buffer-name buffer))
         (filename (buffer-file-name buffer))
         (bookmark (alist-get bookmark-key *eyebrowse-bookmarks* nil t 'string-equal))
         (remark (alist-get "remark" bookmark nil t 'string-equal)))
    (cond
     ((or (not bookmark) cover-p)
      (setq bookmark `((eyebrowse . ,(eyebrowse--get 'current-slot))
                       (buffername . , buffername)
                       (filename . ,filename)
                       ;; copied from bookmark.el
                       ;; (front-context . ,(if (>= (- (point-max) (point))
                       ;;                           eyebrowse-bookmark-search-size)
                       ;;                       (buffer-substring-no-properties
                       ;;                        (point)
                       ;;                        (+ (point) eyebrowse-bookmark-search-size))
                       ;;                     nil))
                       ;; copied from bookmark.el
                       ;; (rear-context . ,(if (>= (- (point) (point-min))
                       ;;                          eyebrowse-bookmark-search-size)
                       ;;                      (buffer-substring-no-properties
                       ;;                       (point)
                       ;;                       (- (point) eyebrowse-bookmark-search-size))
                       ;;                    nil))
                       (line . ,(save-excursion
                                         (beginning-of-line)
                                         (let ((start (point)))
                                           (end-of-line)
                                           (let ((end (point)))
                                             (buffer-substring start end)))))
                       (description . ,(save-excursion
                                         (beginning-of-defun)
                                         (let ((start (point)))
                                           (end-of-line)
                                           (let ((end (point)))
                                             (buffer-substring start end)))))
                       (remark . ,(read-string "Please input the remark of this bookmark: " remark))
                       (position . ,(point))))
      ;; set or append the bookmark of key
      (setf (alist-get bookmark-key *eyebrowse-bookmarks* nil t 'equal) bookmark)
      ;; define the bookmark key
      (eyebrowse-define-bookmark-key bookmark-key)
      (when (not cover-p)
        (message "Add bookmark %s suceed!" bookmark-key)))
     (t
      (message "Bookmark \"%s\" exist, can't override!" bookmark-key)))))

(defun eyebrowse-define-bookmark-key (bookmark-key)
  "Define the BOOKMARK-KEY into eyebrowse-xmonad-mode-map."
  ;; define H-<key>, cycle the bookmarks of <key>
  (define-key eyebrowse-xmonad-mode-map (kbd (format "H-%s" bookmark-key))
    (eval `(toggle-minibuffer
            (lambda ()
              (interactive)
              (eyebrowse-jump-to-bookmark ,bookmark-key)))))
  ;; define H-S-<key>, show bookmark list and choose one to delete
  (define-key eyebrowse-xmonad-mode-map (kbd (format "H-%s" (upcase bookmark-key)))
    (eval `(toggle-minibuffer
            (lambda ()
              (interactive)
              (eyebrowse-delete-bookmark ,bookmark-key)))))
  ;; define H-C-<key>, show bookmark list of <key>
  (define-key eyebrowse-xmonad-mode-map (kbd (format "H-C-%s" bookmark-key))
    (eval `(toggle-minibuffer
            (lambda ()
              (interactive)
              (eyebrowse-list-bookmarks nil (lambda (bookmark)
                                              (string-equal (car bookmark) ,bookmark-key))))))))

(defun eyebrowse-cover-bookmark (bookmark-key)
  "Cover the exist bookmark of BOOKMARK-KEY."
  (interactive "sPlease input the bookmark key to cover: ")
  (message "cover bookmark: %s" bookmark-key)
  (eyebrowse-add-bookmark bookmark-key t)
  (message "Cover bookmark %s suceed!" bookmark-key))

(defun eyebrowse-delete-bookmark (bookmark-key)
  "Delete the exist bookmark of BOOKMARK-KEY."
  (let* ((bookmark (alist-get bookmark-key *eyebrowse-bookmarks* nil t 'equal))
         (remark (cdr (assq 'remark (cdr bookmark)))))
    (when (yes-or-no-p (format "Ensure to delete bookmark %s: %s ? " bookmark-key remark))
      (message "delete bookmark: %s" bookmark-key)
      (let ((bookmark (alist-get bookmark-key *eyebrowse-bookmarks* nil t 'equal)))
        ;; remove bookmark
        (setf (alist-get bookmark-key *eyebrowse-bookmarks* :remove :remove 'equal) :remove)
        ;; undefine bookmark key
        (define-key eyebrowse-xmonad-mode-map (kbd (format "H-%s" bookmark-key)) nil)
        (message "Delete bookmark %s suceed!" bookmark-key)))))

(defun eyebrowse-clear-bookmark ()
  "."
  (interactive)
  (when (yes-or-no-p "Clear all the eyebrowse bookmarks?")
    (setq *eyebrowse-bookmarks* nil)
    (message "Clear eyebrowse bookmarks suceed!")))

(defun eyebrowse-jump-to-bookmark (bookmark-key)
  "Jump to the eyebrowse bookmark with BOOKMARK-KEY."
  (interactive "sPlease input the bookmark key to jump: ")
  (let ((bookmark (alist-get bookmark-key *eyebrowse-bookmarks* nil t 'equal)))
    (when bookmark
      (setq *eyebrowse-last-bookmark-key* *eyebrowse-current-bookmark-key*)
      (setq *eyebrowse-current-bookmark-key* bookmark-key)
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

(defun switch-to-last-eyebrowse-bookmark ()
  "."
  (interactive)
  (when (not (equal *eyebrowse-current-bookmark-key* *eyebrowse-last-bookmark-key*))
    (eyebrowse-jump-to-bookmark *eyebrowse-last-bookmark-key*)))

(defun define-eyebrowse-bookmarks-keymap ()
  "."
  (let* ((keys "abcdefghijklmnopqrstuvwxz',.;")
         (indice (number-sequence 0 (- (length keys) 1))))
    (seq-doseq (idx indice)
      (let ((key (aref keys idx)))
        (define-key eyebrowse-xmonad-mode-map (kbd (format "H-M-%c" key))
          `(lambda ()
             (interactive)
             (eyebrowse-add-bookmark ,(format "%c" key))))))
    nil))

(defun eyebrowse-list-bookmarks (&optional action filter)
  "List the eyebrowse bookmarks with FILTER, choose one and do ACTION on it."
  (interactive)
  (let* ((prompt "Please select the bookmark: ")
         (collections *eyebrowse-bookmarks*)
         (collections (if filter (seq-filter filter collections) collections))
         (collections (mapcar (lambda (bookmark)
                                (let* ((key (car bookmark))
                                       (body (cdr bookmark))
                                       (eyebrowse-slot (cdr (assq 'eyebrowse body)))
                                       (eyebrowse-conf (eyebrowse-get-config-with-slot eyebrowse-slot))
                                       (eyebrowse-conf-string (eyebrowse-config-string eyebrowse-conf))
                                       (buffername (cdr (assq 'buffername body)))
                                       ;; (front-context (cdr (assq 'front-context body)))
                                       ;; (rear-context (cdr (assq 'rear-context body)))
                                       (line (cdr (assq 'line body)))
                                       (description (cdr (assq 'description body)))
                                       (remark (cdr (assq 'remark body)))
                                       (position (cdr (assq 'position body))))
                                  (format "%s: %s\n%s\n%s\t%s\t%s\t%d" key remark line description eyebrowse-conf-string buffername position)))
                              collections))
         (collections (sort collections #'string<))
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
    ("clear  bookmark" . eyebrowse-clear-bookmark)
    ;; TODO
    ("load   bookmark" . eyebrowse-load-bookmark)
    ;; TODO
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
    (define-key map (kbd "H-~") #'switch-to-last-eyebrowse-bookmark)
    (define-key map (kbd "H-1") (define-eyebrowse-extend-switch-config-function 1))
    (define-key map (kbd "H-2") (define-eyebrowse-extend-switch-config-function 2))
    (define-key map (kbd "H-3") (define-eyebrowse-extend-switch-config-function 3))
    (define-key map (kbd "H-4") (define-eyebrowse-extend-switch-config-function 4))
    (define-key map (kbd "H-5") (define-eyebrowse-extend-switch-config-function 5))
    (define-key map (kbd "H-6") (define-eyebrowse-extend-switch-config-function 6))
    (define-key map (kbd "H-7") (define-eyebrowse-extend-switch-config-function 7))
    (define-key map (kbd "H-8") (define-eyebrowse-extend-switch-config-function 8))
    (define-key map (kbd "H-9") (define-eyebrowse-extend-switch-config-function 9))
    (define-key map (kbd "<H-f1>") #'switch-to-scratch-buffer)
    (define-key map (kbd "<H-f2>") #'switch-to-message-buffer)
    (define-key map (kbd "<H-S-return>") #'run-or-raise-next-terminal)
    (define-key map (kbd "<H-tab>") #'goto-next-window)
    (define-key map (kbd "<H-iso-lefttab>") #'goto-previous-window)
    (define-key map (kbd "<H-return>") #'swap-to-main-window)
    (define-key map (kbd "<H-backspace>") #'goto-last-window)
    ;; (define-key map (kbd "H-m") #'goto-main-window)
    (define-key map (kbd "H-M-s") #'swap-window-in-current-frame)
    (define-key map (kbd "H-M-c") #'delete-window)
    ;; (define-key map (kbd "H-c") #'copy-window-in-current-frame)
    (define-key map (kbd "H-<left>") #'windmove-left)
    (define-key map (kbd "H-<right>") #'windmove-right)
    (define-key map (kbd "H-<up>") #'windmove-up)
    (define-key map (kbd "H-<down>") #'windmove-down)
    (define-key map (kbd "H-M-<left>") #'adjust-window-size)
    (define-key map (kbd "H-M-<right>") #'adjust-window-size)
    (define-key map (kbd "H-M-<up>") #'adjust-window-size)
    (define-key map (kbd "H-M-<down>") #'adjust-window-size)
    map)
  "Initial key map for `eyebrowse-xmonad-mode'.")

(defun save-eyebrowse-bookmarks ()
  "."
  (when (> (length *eyebrowse-bookmarks*) 0)
    (with-temp-file +eyebrowse-bookmarks-file-name+
      (let ((print-length nil)
            (print-level nil))
        (print *eyebrowse-bookmarks* (current-buffer))))))

(defun load-eyebrowse-bookmarks ()
  "."
  (when (file-exists-p +eyebrowse-bookmarks-file-name+)
    (with-temp-buffer
      (insert-file-contents +eyebrowse-bookmarks-file-name+)
      (goto-char (point-min))
      (setq *eyebrowse-bookmarks* (read (current-buffer)))
      ;; define bookmark key
      (redefine-eyebrowse-bookmark-keys)))
  t)

(defun redefine-eyebrowse-bookmark-keys ()
  "."
  (dolist (bookmark *eyebrowse-bookmarks*)
    (let ((key (car bookmark)))
      (message "redefine: %s" key)
      (eyebrowse-define-bookmark-key key))))

(define-minor-mode eyebrowse-xmonad-mode
  "Toggle `eyebrowse-xmonad-mode."
  :keymap eyebrowse-xmonad-mode-map
  :global t
  (cond
   (eyebrowse-xmonad-mode
    (message "Now turn on the eyebrowse-xmonad-mode...")
    (define-eyebrowse-bookmarks-keymap)
    (add-hook 'emacs-startup-hook
              (lambda ()
                (when (load-eyebrowse-bookmarks)
                  (add-hook 'kill-emacs-hook 'save-eyebrowse-bookmarks)))))
   (t
    (message "Now turn off the eyebrowse-xmonad-mode..."))))

(provide 'eyebrowse-xmonad)
;;; eyebrowse-xmonad.el ends here
