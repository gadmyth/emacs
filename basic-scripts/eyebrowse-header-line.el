;;; eyebrowse-header-line.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2024 Gadmyth

;; Author: eyebrowse-header-line.el <gadmyth@gmail.com>
;; Version: 1.0
;; Package-Version: 20240812.001
;; Package-Requires: eyebrowse, eyebrowse+, q
;; Keywords: eyebrowse, header-line
;; Homepage: https://www.github.com/gadmyth/emacs
;; URL: https://www.github.com/gadmyth/emacs/blob/master/basic-script/eyebrowse-header-line.el

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
;; eyebrowse-header-line's code can be found here:
;;   https://www.github.com/gadmyth/emacs/blob/master/basic-script/eyebrowse-header-line.el

;;; Commentary:
;;; Code:

(require 'eyebrowse+)
(require 'q)

(defvar *eyebrowse-show-header-line* nil)

(define-debug-message eyebrowse-header-line)

(defvar eyebrowse-config-format '(:eval (eyebrowse-header-line-indicator)))

;; copied from eyebrowse-mode-line-indicator and modified a little
(defun eyebrowse-header-line-indicator ()
  "Return a string representation of the window configurations."
  (let* ((left-delimiter (propertize eyebrowse-mode-line-left-delimiter
                                     'face 'eyebrowse-mode-line-delimiters))
         (right-delimiter (propertize eyebrowse-mode-line-right-delimiter
                                      'face 'eyebrowse-mode-line-delimiters))
         (separator (propertize eyebrowse-mode-line-separator
                                'face 'eyebrowse-mode-line-separator))
         (current-slot (eyebrowse--get 'current-slot))
         (window-configs (if (eq eyebrowse-mode-line-style 'current)
                             (list (assoc current-slot (eyebrowse--get 'window-configs)))
                           (eyebrowse--get 'window-configs))))
    (if (and eyebrowse-mode-line-style
             (not (eq eyebrowse-mode-line-style 'hide))
             (or (and (not (eq eyebrowse-mode-line-style 'smart))
                      eyebrowse-mode-line-style)
                 (and (eq eyebrowse-mode-line-style 'smart)
                      (> (length window-configs) 1))))
        (concat
         left-delimiter
         (mapconcat
          (lambda (window-config)
            (let* ((slot (car window-config))
                   (face (if (= slot current-slot)
                             'eyebrowse-mode-line-active
                           'eyebrowse-mode-line-inactive))
                   (keymap
                    (let ((map (make-sparse-keymap)))
                      (define-key map (kbd "<header-line><mouse-1>")
                                  ;; modified for "Symbol’s value as variable is void: slot"
                                  `(lambda (_e)
                                     (interactive "e")
                                     (eyebrowse-switch-to-window-config ,slot)))
                      map))
                   (help-echo "mouse-1: Switch to indicated workspace")
                   (caption (eyebrowse-format-slot window-config)))
              (propertize caption 'face face 'slot slot
                          'mouse-face 'mode-line-highlight
                          'local-map keymap
                          'help-echo help-echo)))
          window-configs separator)
         right-delimiter)
      "")))


(defun set-eyebrowse-header-line-format ()
  "."
  (interactive)
  (when (or (not header-line-format)
            current-prefix-arg)
    (when (window-live-p (get-buffer-window))
      (with-current-buffer (current-buffer)
        (eyebrowse-header-line-debug-message "set-eyebrowse-header-line-format, buffer: %s" (current-buffer))
        (setq-local header-line-format eyebrowse-config-format)))))

(defun reset-eyebrowse-header-line-format ()
  "."
  (interactive)
  (when (not (equal header-line-format eyebrowse-config-format))
    (eyebrowse-header-line-debug-message "reset-eyebrowse-header-line-format, buffer: %s" (current-buffer))
    (setq-local header-line-format eyebrowse-config-format)))

(defun force-reset-eyebrowse-header-line-format ()
  "."
  (interactive)
  (eyebrowse-header-line-debug-message "force-reset-eyebrowse-header-line-format, buffer: %s" (current-buffer))
  (setq-local header-line-format eyebrowse-config-format))

(defvar eyebrowse-header-line-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "H-~") 'force-reset-eyebrowse-header-line-format)
    map)
  "Initial key map for `eyebrowse-header-line-mode'.")

(defmacro walk-all-frame-windows (&rest body)
  "Walk all windows in all frame for each execute the BODY."
  `(walk-windows (lambda (window)
                   (with-current-buffer (window-buffer window)
                     ,@body))
                 nil t))

(define-minor-mode eyebrowse-header-line-mode
  "Toggle `eyebrowse-header-line-mode."
  :keymap eyebrowse-header-line-mode-map
  :global t
  (cond
   (eyebrowse-header-line-mode
    (message "Now turn on the eyebrowse-header-line-mode...")
    (add-hook 'eyebrowse-lazy-load-hook #'set-eyebrowse-header-line-format)
    (add-hook 'find-file-hook #'set-eyebrowse-header-line-format)
    (add-hook 'window-configuration-change-hook #'set-eyebrowse-header-line-format)
    (add-hook 'eyebrowse-post-window-switch-hook #'reset-eyebrowse-header-line-format)
    ))
  (t
   (message "Now turn off the eyebrowse-header-line-mode...")
   ;; header-line-format hook
   (remove-hook 'find-file-hook #'set-eyebrowse-header-line-format)
   (remove-hook 'window-configuration-change-hook #'set-eyebrowse-header-line-format)
   (remove-hook 'eyebrowse-post-window-switch-hook #'reset-eyebrowse-header-line-format)
   (walk-all-frame-windows
    (setq-local header-line-format nil))
   ))

(provide 'eyebrowse-header-line)
;;; eyebrowse-header-line.el ends here
