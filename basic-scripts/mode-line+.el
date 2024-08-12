;;; mode-line+.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2024 Gadmyth

;; Author: mode-line+.el <gadmyth@gmail.com>
;; Version: 1.0
;; Package-Version: 20240812.001
;; Package-Requires: q, network-util, weathers
;; Keywords: mode line
;; Homepage: https://www.github.com/gadmyth/emacs
;; URL: https://www.github.com/gadmyth/emacs/blob/master/basic-script/mode-line+.el

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
;; mode-line+'s code can be found here:
;;   https://www.github.com/gadmyth/emacs/blob/master/basic-script/mode-line+.el

;;; Commentary:
;;; Code:


(require 'q)
(require 'network-util)
(require 'weathers)


(define-debug-message mode-line-plus)

(defvar mode-line-plus-format
  '("%e"
    (:eval
     (when (featurep 'window-numbering)
       (window-numbering-get-number-string)))
    mode-line-front-space
    mode-line-mule-info
    mode-line-client
    mode-line-modified
    mode-line-remote
    " "
    (:eval (propertized-buffer-identification "%b"))
    " "
    mode-line-percent-position
    " "
    "(%l,%c)"
    vc-mode
    " | "
    ;; time info
    (:eval (time-info-format))
    ;; local ip
    (:eval (current-ip-with-seperator))
    ;; public ip
    (:eval (public-ip-with-seperator))
    ;; weather
    (:eval (weather-with-seperator))))

(defface mode-line-plus-time-face
  '((((class color) (background light)) (:foreground "sea green" :weight bold))
    (((class color) (background dark)) (:foreground "SteelBlue3" :weight bold)))
  "Face for mode line plus time."
  :group 'modeline+)

(defun time-info-format ()
  "."
  (let* ((time-string (format-time-string "%Y-%m-%d %H:%M %a" (current-time)))
         (time-string-list (s-split " " time-string))
         (date-string (car time-string-list))
         (time-string (propertize (cadr time-string-list) 'face 'mode-line-plus-time-face))
         (week-string (caddr time-string-list)))
    (format "%s %s %s" date-string time-string week-string)))

(defun current-ip-with-seperator ()
  "."
  (let ((ip (current-ip)))
    (if (> (length ip) 0) (list " | " ip) "")))

(defun public-ip-with-seperator ()
  "."
  (let ((ip (fetched-public-ip)))
    (if (> (length ip) 0) (list " | " ip) "")))

(defun weather-with-seperator ()
  "."
  (let ((weather (fetched-weather)))
    (if (> (length weather) 0) (list " | " weather) "")))

(defun set-mode-line-plus-format ()
  "."
  (interactive)
  (when (or (not (equal mode-line-format mode-line-plus-format))
            current-prefix-arg)
    (when (window-live-p (get-buffer-window))
      (with-current-buffer (current-buffer)
        (mode-line-plus-debug-message "set-mode-line-plus-format, buffer: %s" (current-buffer))
        (setq-local mode-line-format mode-line-plus-format)))))

(defun reset-mode-line-plus-format ()
  "."
  (interactive)
  (when (not (equal mode-line-format mode-line-plus-format))
    (mode-line-plus-debug-message "reset-mode-line-plus-format, buffer: %s" (current-buffer))
    (setq-local mode-line-format mode-line-plus-format)))

(defun force-reset-mode-line-plus-format ()
  "."
  (interactive)
  (mode-line-plus-debug-message "force-reset-mode-line-plus-format, buffer: %s" (current-buffer))
  (setq-local mode-line-format mode-line-plus-format))

(defvar mode-line-plus-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Initial key map for `mode-line-plus-mode'.")

(define-minor-mode mode-line-plus-mode
  "Toggle `mode-line-plus-mode."
  :keymap mode-line-plus-mode-map
  :global t
  (cond
   (mode-line-plus-mode
    (message "turn on the mode-line-plus-mode")
    (set-mode-line-plus-format)
    ;; set mode-line-buffer-identification
    ;; (setq-default mode-line-buffer-identification eyebrowse-buffer-name-format)
    (add-hook 'find-file-hook #'set-mode-line-plus-format)
    (add-hook 'window-configuration-change-hook #'set-mode-line-plus-format)
    ;; (add-hook 'eyebrowse-post-window-switch-hook #'reset-mode-line-plus-format)
    )
   (t
    (message "turn off the mode-line-plus-mode")
    (remove-hook 'find-file-hook #'set-mode-line-plus-format)
    (remove-hook 'window-configuration-change-hook #'set-mode-line-plus-format)
    ;; (remove-hook 'eyebrowse-post-window-switch-hook #'reset-mode-line-plus-format)
    )
   ))

(provide 'mode-line+)
;;; mode-line+.el ends here
