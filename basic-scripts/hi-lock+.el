;;; hi-lock+.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2022 Gadmyth

;; Author: hi-lock+.el <gadmyth@gmail.com>
;; Version: 1.0.2
;; Package-Version: 20221127.001
;; Package-Requires:
;; Keywords: hi-lock, highlight
;; Homepage: https://www.github.com/gadmyth/emacs
;; URL: https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/hi-lock+.el

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
;; hi-lock+'s code can be found here:
;;   https://www.github.com/gadmyth/emacs/blob/master/basic-scripts/hi-lock+.el

;;; Commentary:
;;; Code:

(require 'hi-lock)
(require 'thingatpt+)

(defvar *hi-lock-toggle-superword-p* nil)
(defvar *hi-lock-word-bound-p* nil)

(defun hi-lock/toggle-highlight-at-point ()
  "."
  (interactive)
  (let* ((ev last-command-event)
         (base (event-basic-type ev)))
    (message "base: %S" base)
    (pcase base
      (?. (hi-lock/do-highlight-at-point))
      (?s (hi-lock/redo-highlight-at-point))
      (?w (hi-lock/redo-highlight-at-point))
      (?u (hi-lock/clear-current-highlight))
      (?c (hi-lock/clear-all-highlight))
      (?n (hi-lock/next-heighlight-at-point))
      (?p (hi-lock/previous-heighlight-at-point))
      (_ nil)))
  (message "toggle-superword-p: %S, word bound: %S; s: toggle superword, w: toggle word bound, c: clear all, n: next, p: previous"
           *hi-lock-toggle-superword-p* *hi-lock-word-bound-p*)
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (vector (list ?.))
                 (lambda () (interactive)
                   (hi-lock/toggle-highlight-at-point)))
     (define-key map (vector (list ?s))
                 (lambda () (interactive)
                   (setq *hi-lock-toggle-superword-p* (not *hi-lock-toggle-superword-p*))
                   (hi-lock/toggle-highlight-at-point)))
     (define-key map (vector (list ?w))
                 (lambda () (interactive)
                   (setq *hi-lock-word-bound-p* (not *hi-lock-word-bound-p*))
                   (hi-lock/toggle-highlight-at-point)))
     (define-key map (vector (list ?u))
                 (lambda () (interactive)
                   (hi-lock/toggle-highlight-at-point)))
     (define-key map (vector (list ?u))
                 (lambda () (interactive)
                   (hi-lock/toggle-highlight-at-point)))
     (define-key map (vector (list ?\c))
                 (lambda () (interactive)
                   (hi-lock/toggle-highlight-at-point)))
     (define-key map (vector (list ?n))
                 (lambda () (interactive)
                   (hi-lock/toggle-highlight-at-point)))
     (define-key map (vector (list ?p))
                 (lambda () (interactive)
                   (hi-lock/toggle-highlight-at-point)))
     map)))

(defun hi-lock/do-highlight-at-point ()
  "Toggle highlight at point."
  (interactive)
  (let ((regexp (car (hi-lock--regexps-at-point)))
        (hi-lock-auto-select-face t))
    (when (not regexp)
      (highlight-phrase (region-or-word-at-point *hi-lock-toggle-superword-p* *hi-lock-word-bound-p*) (hi-lock-read-face-name)))
    (goto-char (car (bound-at-point *hi-lock-toggle-superword-p*)))
    (deactivate-mark)))

(defun hi-lock/redo-highlight-at-point ()
  "Toggle highlight at point."
  (interactive)
  (let ((regexp (car (hi-lock--regexps-at-point)))
        (hi-lock-auto-select-face t))
    (when regexp
      (unhighlight-regexp regexp)
      (highlight-phrase (region-or-word-at-point *hi-lock-toggle-superword-p* *hi-lock-word-bound-p*) (hi-lock-read-face-name)))
    (deactivate-mark)))

(defun hi-lock/clear-current-highlight ()
  "Clear all highlight."
  (interactive)
  (when-let ((regexp (car (hi-lock--regexps-at-point))))
    (unhighlight-regexp regexp)))

(defun hi-lock/clear-all-highlight ()
  "Clear all highlight."
  (interactive)
  (let ((hi-regexp-list (mapcar #'car hi-lock-interactive-patterns)))
    (mapcar 'unhighlight-regexp hi-regexp-list)))

(defun hi-lock/next-heighlight-at-point ()
  "."
  (interactive)
  (let ((regexp (car (hi-lock--regexps-at-point))))
    (message "regexp: %s" regexp)
    (when regexp
      (let ((case-fold-search nil))
        (when (re-search-forward regexp nil t 2)
          (goto-char (match-beginning 0)))))))

(defun hi-lock/previous-heighlight-at-point ()
  "."
  (interactive)
  (let ((regexp (car (hi-lock--regexps-at-point))))
    (message "regexp: %s" regexp)
    (when regexp
      (let ((case-fold-search nil))
        (when (re-search-backward regexp nil t)
          (goto-char (match-beginning 0)))))))

(global-set-key (kbd "C-S-h .") 'hi-lock/toggle-highlight-at-point)
(global-set-key (kbd "C-S-h u") 'hi-lock/toggle-highlight-at-point)
(global-set-key (kbd "C-S-h s") 'hi-lock/toggle-highlight-at-point)
(global-set-key (kbd "C-S-h n") 'hi-lock/toggle-highlight-at-point)
(global-set-key (kbd "C-S-h p") 'hi-lock/toggle-highlight-at-point)
(global-set-key (kbd "C-S-h c") 'hi-lock/clear-all-highlight)

(provide 'hi-lock+)
;;; hi-lock+.el ends here
