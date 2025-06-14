;;; eyebrowse-xmonad.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2021 gadmyth

;; Author: eyebrowse-xmonad.el <gadmyth@gmail.com>
;; Version: 1.1.1
;; Package-Version: 20250604.001
;; Package-Requires: eyebrowse+, s, windows
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


(require 'eyebrowse+)
(require 'windows)


(defun eyebrowse-switch-to-last-config ()
  "."
  (interactive)
  (when-let ((last-slot (eyebrowse--get 'last-slot)))
    (eyebrowse-switch-to-window-config last-slot)))

(defmacro define-eyebrowse-extend-switch-config-function (slot)
  "SLOT."
  ``(defun ,(intern (format "eyebrowse-switch-to-window-config-%d-with-tag" ,slot)) ()
     (interactive)
     (if (not (eyebrowse--window-config-present-p ,,slot))
         (let ((tag (read-string (format "Set tag for the new eyebrowse config of slot %d: " ,,slot))))
           (eyebrowse-switch-to-window-config ,,slot)
           (eyebrowse-rename-window-config ,,slot tag))
       (eyebrowse-switch-to-window-config ,,slot))))

(defvar eyebrowse-xmonad-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "H-`") #'eyebrowse-switch-to-last-config)
    (cl-loop
     for w in (number-sequence 1 22)
     for k in (seq-concatenate
               'list
               (number-sequence 1 9)
               '(0)
               (mapcar (lambda (i) (format "f%d" i)) (number-sequence 1 12)))
     do
     (define-key map (kbd (format "<H-%S>" k)) (define-eyebrowse-extend-switch-config-function w)))
    (define-key map (kbd "<H-S-return>") #'run-or-raise-next-terminal)
    (define-key map (kbd "<H-tab>") #'goto-next-window)
    (define-key map (kbd "<H-iso-lefttab>") #'goto-previous-window)
    (define-key map (kbd "<H-return>") #'swap-to-main-window)
    (define-key map (kbd "<H-backspace>") #'goto-last-window)
    (define-key map (kbd "H-m") #'goto-main-window)
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

(define-minor-mode eyebrowse-xmonad-mode
  "Toggle `eyebrowse-xmonad-mode."
  :keymap eyebrowse-xmonad-mode-map
  :global t
  (cond
   (eyebrowse-xmonad-mode
    (message "turn on the eyebrowse-xmonad-mode"))
   (t
    (message "turn off the eyebrowse-xmonad-mode"))))

(provide 'eyebrowse-xmonad)
;;; eyebrowse-xmonad.el ends here
