;;; bookmarks.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2021 gadmyth

;; Author: bookmarks.el <gadmyth@gmail.com>
;; Version: 1.0.1
;; Package-Version: 20240812.001
;; Package-Requires: s, minibuffer+
;; Keywords: bookmark
;; Homepage: https://www.github.com/gadmyth/emacs
;; URL: https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/bookmarks.el

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
;; bookmarks's code can be found here:
;;   https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/bookmarks.el

;;; Commentary:
;;; Code:


(require 's)

(require 'minibuffer+)

(defvar *bookmarks* nil)

(defvar *bookmarks-current-bookmark-key* nil)

(defvar *bookmarks-last-bookmark-key* nil)

(defconst +bookmarks-file-name+ (expand-file-name "~/.emacs.bookmarks"))

(defvar bookmarks-search-size 16)

(defvar *bookmarks-initialized* nil)

(defun bookmarks-add-bookmark (bookmark-key &optional cover-p)
  "Set the current position as bookmark with BOOKMARK-KEY, if COVER-P, overwrite the exist bookmark."
  (interactive "sPlease input the bookmark key to add: ")
  (let* ((buffer (current-buffer))
         (buffername (buffer-name buffer))
         (filename (buffer-file-name buffer))
         (bookmark (alist-get bookmark-key *bookmarks* nil t 'string-equal))
         (remark (alist-get "remark" bookmark nil t 'string-equal)))
    (cond
     ((or (not bookmark) cover-p)
      (setq bookmark `((buffername . , buffername)
                       (filename . ,filename)
                       ;; copied from bookmark.el
                       ;; (front-context . ,(if (>= (- (point-max) (point))
                       ;;                           bookmarks-search-size)
                       ;;                       (buffer-substring-no-properties
                       ;;                        (point)
                       ;;                        (+ (point) bookmarks-search-size))
                       ;;                     nil))
                       ;; copied from bookmark.el
                       ;; (rear-context . ,(if (>= (- (point) (point-min))
                       ;;                          bookmarks-search-size)
                       ;;                      (buffer-substring-no-properties
                       ;;                       (point)
                       ;;                       (- (point) bookmarks-search-size))
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
                       (remark . ,(read-string "Now add a bookmark, please input the remark: " remark))
                       (position . ,(point))))
      ;; set or append the bookmark of key
      (setf (alist-get bookmark-key *bookmarks* nil t 'equal) bookmark)
      ;; define the bookmark key
      (bookmarks-define-bookmark-key bookmark-key)
      (when (not cover-p)
        (message "Add bookmark %s suceed!" bookmark-key)))
     (t
      (message "Bookmark \"%s\" exist, can't override!" bookmark-key)))))

(defun bookmarks-define-bookmark-key (bookmark-key)
  "Define the BOOKMARK-KEY into bookmarks-xmonad-mode-map."
  ;; define H-<key>, cycle the bookmarks of <key>
  (define-key bookmarks-mode-map (kbd (format (pcase system-type
                                                ('darwin "s-%s")
                                                (_ "H-%s"))
                                              bookmark-key))
    (eval `(toggle-minibuffer
            (lambda ()
              (interactive)
              (bookmarks-jump-to-bookmark ,bookmark-key)))))
  ;; define H-S-<key>, show bookmark list and choose one to delete
  (define-key bookmarks-mode-map (kbd (format (pcase system-type
                                                       ('darwin "s-C-%s")
                                                       (_ "H-%s"))
                                                     (upcase bookmark-key)))
    (eval `(toggle-minibuffer
            (lambda ()
              (interactive)
              (bookmarks-delete-bookmark ,bookmark-key)))))
  ;; define H-C-<key>, show bookmark list of <key>
  (define-key bookmarks-mode-map (kbd (format "H-C-%s" bookmark-key))
    (eval `(toggle-minibuffer
            (lambda ()
              (interactive)
              (bookmarks-list-bookmarks nil (lambda (bookmark)
                                              (string-equal (car bookmark) ,bookmark-key))))))))

(defun bookmarks-cover-bookmark (bookmark-key)
  "Cover the exist bookmark of BOOKMARK-KEY."
  (interactive "sPlease input the bookmark key to cover: ")
  (message "cover bookmark: %s" bookmark-key)
  (bookmarks-add-bookmark bookmark-key t)
  (message "Cover bookmark %s suceed!" bookmark-key))

(defun bookmarks-delete-bookmark (bookmark-key)
  "Delete the exist bookmark of BOOKMARK-KEY."
  (let* ((bookmark (alist-get bookmark-key *bookmarks* nil t 'equal))
         (remark (cdr (assq 'remark (cdr bookmark)))))
    (when (yes-or-no-p (format "Ensure to delete bookmark %s: %s ? " bookmark-key remark))
      (message "delete bookmark: %s" bookmark-key)
      (let ((bookmark (alist-get bookmark-key *bookmarks* nil t 'equal)))
        ;; remove bookmark
        (setf (alist-get bookmark-key *bookmarks* :remove :remove 'equal) :remove)
        ;; undefine bookmark key
        (define-key bookmarks-mode-map (kbd (format "H-%c" bookmark-key)) nil)
        (message "Delete bookmark %s suceed!" bookmark-key)))))

(defun bookmarks-clear-bookmark ()
  "."
  (interactive)
  (when (yes-or-no-p "Clear all the bookmarks bookmarks?")
    (setq *bookmarks* nil)
    (message "Clear bookmarks bookmarks suceed!")))

(defun bookmarks-jump-to-bookmark (bookmark-key)
  "Jump to the bookmarks bookmark with BOOKMARK-KEY."
  (interactive "sPlease input the bookmark key to jump: ")
  (let ((bookmark (alist-get bookmark-key *bookmarks* nil t 'equal)))
    (when bookmark
      (setq *bookmarks-last-bookmark-key* *bookmarks-current-bookmark-key*)
      (setq *bookmarks-current-bookmark-key* bookmark-key)
      (let* ((buffername (cdr (assq 'buffername bookmark)))
             (window (get-buffer-window buffername))
             (filename (cdr (assq 'filename bookmark)))
             (position (cdr (assq 'position bookmark))))        
        (cond
         (window
          (select-window window))
         ((buffer-live-p (get-buffer buffername))
          (switch-to-buffer buffername))
         ((file-exists-p filename)
          (find-file filename)))
        (goto-char position)))))

(defun bookmarks-switch-to-last-bookmark ()
  "."
  (interactive)
  (when (not (equal *bookmarks-current-bookmark-key* *bookmarks-last-bookmark-key*))
    (bookmarks-jump-to-bookmark *bookmarks-last-bookmark-key*)))

(defun define-bookmarks-keymap ()
  "."
  (let* ((keys "abcdefghijklmnopqrstuvwxz',.;")
         (indice (number-sequence 0 (- (length keys) 1))))
    (seq-doseq (idx indice)
      (let ((key (aref keys idx)))
        (define-key bookmarks-mode-map (kbd (format (pcase system-type
                                                      ('darwin "s-M-%c")
                                                      (_ "H-M-%c"))
                                                      key))
          `(lambda ()
             (interactive)
             (bookmarks-add-bookmark ,(format "%c" key))))))
    nil))

(defun bookmarks-list-bookmarks (&optional action filter)
  "List the bookmarks with FILTER, choose one and do ACTION on it."
  (interactive)
  (let* ((prompt "Please select the bookmark: ")
         (collections *bookmarks*)
         (collections (if filter (seq-filter filter collections) collections))
         (collections (mapcar (lambda (bookmark)
                                (let* ((key (car bookmark))
                                       (body (cdr bookmark))
                                       ;; (buffername (cdr (assq 'buffername body)))
                                       ;; (front-context (cdr (assq 'front-context body)))
                                       ;; (rear-context (cdr (assq 'rear-context body)))
                                       ;; (line (cdr (assq 'line body)))
                                       ;; (description (cdr (assq 'description body)))
                                       (remark (cdr (assq 'remark body)))
                                       ;; (position (cdr (assq 'position body)))
                                       )
                                  (format "%s: %s" key remark)))
                              collections))
         (collections (sort collections #'string<))
         (default (car collections))
         (selected (completing-read prompt collections nil t nil nil default))
         (bookmark-key (substring selected 0 (s-index-of ":" selected))))
    (cond
     (action
      (apply action (list bookmark-key)))
     (t
      (bookmarks-jump-to-bookmark bookmark-key)))))

(defvar *bookmarks-modify-action-alist*
  '(("cover  bookmark" . (lambda ()
                           (interactive)
                           (bookmarks-list-bookmarks 'bookmarks-cover-bookmark)))
    ("delete bookmark" . (lambda ()
                           (interactive)
                           (bookmarks-list-bookmarks 'bookmarks-delete-bookmark)))
    ("clear  bookmark" . bookmarks-clear-bookmark)
    ;; TODO
    ("load   bookmark" . bookmarks-load-bookmark)
    ;; TODO
    ("save   bookmark" . bookmarks-save-bookmark)))

(defun bookmarks-modify-bookmark ()
  "."
  (interactive)
  (let* ((prompt "modify bookmark, please select the bookmark: ")
         (default (caar *bookmarks-modify-action-alist*))
         (action (completing-read prompt *bookmarks-modify-action-alist* nil t nil nil default))
         (func (alist-get action *bookmarks-modify-action-alist* nil nil #'string=)))
    (call-interactively func)))

(defun save-bookmarks ()
  "."
  (when (> (length *bookmarks*) 0)
    (with-temp-file +bookmarks-file-name+
      (let ((print-length nil)
            (print-level nil))
        (print *bookmarks* (current-buffer))))))

(defun load-bookmarks ()
  "."
  (when (file-exists-p +bookmarks-file-name+)
    (with-temp-buffer
      (insert-file-contents +bookmarks-file-name+)
      (goto-char (point-min))
      (setq *bookmarks* (read (current-buffer)))
      ;; define bookmark key
      (redefine-bookmark-keys)))
  t)

(defun redefine-bookmark-keys ()
  "."
  (dolist (bookmark *bookmarks*)
    (let ((key (car bookmark)))
      (message "redefine: %s" key)
      (bookmarks-define-bookmark-key key))))

(defvar bookmarks-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Initial key map for `bookmarks-mode'.")

(define-minor-mode bookmarks-mode
  "Toggle `bookmarks-mode."
  :keymap bookmarks-mode-map
  :global t
  (cond
   (bookmarks-mode
    (message "turn on bookmarks-mode")
    (unless *bookmarks-initialized*
      (define-bookmarks-keymap)
      (when (load-bookmarks)
        (add-hook 'kill-emacs-hook 'save-bookmarks))
      (setq *bookmarks-initialized* t)))
   (t
    (message "turn off bookmarks-mode"))))

(provide 'bookmarks)
;;; bookmarks.el ends here
