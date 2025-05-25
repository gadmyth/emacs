;;; wcy-desktop.el --- faster than desktop.el and less features.

;; Copyright (C) 2020-2025 gadmyth

;; Author:  <chunye.wang@nsn.com>
;; Modified by: <gadmyth@gmail.com>
;; Keywords: convenience

;; Version: 2.0.6
;; Package-Version: 20250525.001

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; save the desktop, i.e. save the opened file. no other features mentioned in
;; desktop.el. in order to make it fater, the file is not really loaded, it is
;; only loaded after you press any key.
;; installation: 
;; (require 'wcy-desktop) 
;; (wcy-destop-init)
;;; Code:

;;;###autoload 
(defvar wcy-desktop-file-name "~/.wcy_desktop_save")
(add-to-list 'auto-coding-alist '("\\.wcy_desktop_save\\'" . utf-8))

(defvar wcy-desktop-key-map nil)

(when (null wcy-desktop-key-map)
  (setq wcy-desktop-key-map (make-keymap))
  (define-key wcy-desktop-key-map (kbd "C-x") ctl-x-map)
  (fillarray (cadr wcy-desktop-key-map) 'wcy-desktop-load-file))

(defun  wcy-desktop-on-kill-emacs ()
  "Save the buffer list, this should be part of `kill-emacs-hook."
  (with-temp-file wcy-desktop-file-name
    (let ((print-length nil)
          (print-level nil))
      (print
       (mapcar #'(lambda(b)
                   (with-current-buffer b
                     (cond ((buffer-file-name b)
                            (list :type 'file
                                  :directory default-directory
                                  :name (buffer-name b)
                                  :path buffer-file-name
                                  :major-mode major-mode))
                           ((get-buffer-process b)
                            (let* ((process (get-buffer-process b))
                                   (command (process-command process)))
                              (list :type 'process
                                    :directory default-directory
                                    :command command
                                    :name (buffer-name b)
                                    :major-mode major-mode)))
                           (t
                            (list :type 'buffer
                                  :name (buffer-name b)
                                  :major-mode major-mode)))))
               (buffer-list))
       (current-buffer)))))

(defun  wcy-desktop-init ()
  "This function install the wcy-desktop.  Put it (wcy-desktop-init) in your ~/.emacs."
  (add-hook 'kill-emacs-hook 'wcy-desktop-on-kill-emacs)
  (wcy-desktop-open-last-opened-files))

(defun  wcy-desktop-open-last-opened-files ()
  "Open files which are still open in last session."
  (when (file-readable-p wcy-desktop-file-name)
    (with-temp-buffer
      (insert-file-contents wcy-desktop-file-name)
      (goto-char (point-min))
      (dolist (x (read (current-buffer)))
        (let ((type (plist-get x :type))
              (buffer-name (plist-get x :name))
              (buffer-major-mode (plist-get x :major-mode)))
          (message "wcy desktop open last opened files, buffer name: %s, type: %s, major mode: %s"
                   buffer-name type buffer-major-mode)
          (cond
           ((eq type 'file)
            (let* ((directory (plist-get x :directory))
                   (file-name (plist-get x :path)))
              (wcy-desktop-prepare-buffer directory file-name)))
           ((eq type 'process)
            (let ((directory (plist-get x :directory)))
              (let ((default-directory directory))
                (cond
                 ((eq buffer-major-mode 'term-mode)
                  (message "wcy new create multi-term...")
                  (unless (featurep 'multi-term)
                    (require-package 'multi-term))
                  (when (featurep 'multi-term)
                    (multi-term)))
                 ((string-equal "*ielm*" buffer-name)
                  (message "wcy new create ielm...")
                  (ielm))
                 (t
                  (message "The type is process, TODO"))))))
           ((eq type 'buffer)
            (when (eq buffer-major-mode 'eshell-mode)
              (message "wcy now should create eshell...")
              (eshell))
            (let ((buffer (get-buffer buffer-name)))
              (when (and buffer (buffer-live-p buffer))
                (when (string-equal "*Message*" buffer-name)
                  (switch-to-buffer buffer)))))
           (t
            (message "Unrecognized type: %S" type))))))))

(defun wcy-desktop-prepare-buffer (directory file-name)
  "Prepare the wcy buffer of FILE-NAME in DIRECTORY."
  (when (file-readable-p file-name)
    (let ((buffer (or (get-file-buffer file-name)
                      (create-file-buffer file-name))))
      (with-current-buffer buffer
        (insert "THE BUFFER IS NOT LOADED YET. PRESS ANY KEY TO LOAD IT.")
        (goto-char 1)
        (set (make-local-variable 'wcy-desktop-is-buffer-loaded) nil)
        (use-local-map wcy-desktop-key-map)
        (setq default-directory  directory
              buffer-file-name file-name
              major-mode 'not-loaded-yet
              buffer-read-only t
              mode-name  "not loaded yet")
        (set-buffer-modified-p nil)))))

(defun  wcy-desktop-load-file (&optional buffer)
  "Load file by reverting BUFFER."
  (interactive)
  (setq buffer (or buffer (current-buffer)))
  (with-current-buffer buffer
    (when (local-variable-p 'wcy-desktop-is-buffer-loaded)
      (message "wcy desktop: %s is loaded" buffer-file-name)
      (revert-buffer nil t nil)
      (when (eq major-mode 'not-loaded-yet)
        (fundamental-mode)))))

(defun wcy-desktop-load-all-files ()
  "Load all files."
  (interactive)
  (mapcar #'(lambda(b) (with-current-buffer b
                         (when (local-variable-p 'wcy-desktop-is-buffer-loaded)
                           (revert-buffer nil t nil))))
          (remove-if-not 'buffer-file-name (buffer-list))))

(provide 'wcy-desktop)
;;; wcy-desktop.el ends here
