;;; hi-lock+.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2022 Gadmyth

;; Author: hi-lock+.el <gadmyth@gmail.com>
;; Version: 1.0.4
;; Package-Version: 20260125.001
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
(defvar *hi-lock-debug* nil)

(defun hi-lock-toggle-debug ()
  "."
  (interactive)
  (setq *hi-lock-debug* (not *hi-lock-debug*))
  (message "turn %s the *hi-lock-debug*" (if *hi-lock-debug* "on" "off")))

(defmacro hi-lock-message (format-string &rest ARGS)
  "If debug is open, send message with FORMAT-STRING and ARGS."
  `(if *hi-lock-debug*
       (message ,format-string ,@ARGS)))

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

(defun hi-lock/toggle-superword-at-point ()
  "Toggle superword at point."
  (interactive)
  (setq *hi-lock-toggle-superword-p* (not *hi-lock-toggle-superword-p*))
  (hi-lock/redo-highlight-at-point))

(defun hi-lock/toggle-word-bound-at-point ()
  "Toggle word bound at point."
  (interactive)
  (setq *hi-lock-word-bound-p* (not *hi-lock-word-bound-p*))
  (hi-lock/redo-highlight-at-point))

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
    (hi-lock-message "regexp: %s" regexp)
    (when regexp
      (let ((case-fold-search nil))
        (when (re-search-forward regexp nil t 2)
          (goto-char (match-beginning 0)))))))

(defun hi-lock/previous-heighlight-at-point ()
  "."
  (interactive)
  (let ((regexp (car (hi-lock--regexps-at-point))))
    (hi-lock-message "regexp: %s" regexp)
    (when regexp
      (let ((case-fold-search nil))
        (when (re-search-backward regexp nil t)
          (goto-char (match-beginning 0)))))))

(defvar hi-lock-plus-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(?.)] 'hi-lock/do-highlight-at-point)
    (define-key map [(?c)] 'hi-lock/clear-all-highlight)
    (define-key map [(?n)] 'hi-lock/next-heighlight-at-point)
    (define-key map [(?p)] 'hi-lock/previous-heighlight-at-point)
    (define-key map [(?s)] 'hi-lock/toggle-superword-at-point)
    (define-key map [(?w)] 'hi-lock/toggle-word-bound-at-point)
    (define-key map [(?u)] 'hi-lock/clear-current-highlight)
    map))

(defvar hi-lock-plus-mode-map (make-sparse-keymap))
  
(define-key hi-lock-plus-mode-map (kbd "C-H") hi-lock-plus-command-map)
(map-keymap (lambda (_key cmd)
              (put cmd 'repeat-map 'hi-lock-plus-command-map))
            hi-lock-plus-command-map)

(define-minor-mode hi-lock-plus-mode
  "Toggle hi-lock-plus-mode."
  :keymap hi-lock-plus-mode-map
  :global t
  (cond
   (hi-lock-plus-mode
    (message "turn on the hi-lock-plus-mode"))
   (t
    (message "turn off the hi-lock-plus-mode"))))

(provide 'hi-lock+)
;;; hi-lock+.el ends here
