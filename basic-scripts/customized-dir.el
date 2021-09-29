;;; customized-dir.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2020 gadmyth

;; Author: customized-dir.el <gadmyth@gmail.com}>
;; Version: 1.0.1
;; Package-Version: 20210929.001
;; Package-Requires: ivy, counsel
;; Keywords: customized-dir.el
;; Homepage: https://www.github.com/gadmyth/emacs
;; URL: https://www.github.com/gadmyth/emacs/blob/master/basic-script/customer-dir.el

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
;; customized-dir's code can be found here:
;;   https://www.github.com/gadmyth/emacs/blob/master/basic-script/customer-dir.el

;;; Commentary:
;;; Code:



(provide 'customized-dir)
;;; customized-dir.el ends here

(require 'ivy)
(require 'counsel)

(defmacro wrap-function-with-default-directory (func)
  "Call the FUNC with `default-directory as DIR."
  `(lambda (dir)
     (let ((default-directory dir))
       (call-interactively ,func))))

(defun switch-to-customized-dir (&rest _)
  "."
  (interactive)
  (ivy-read "Switch to dir: " *customized-dir*
            :action (lambda (dir)
                      (ivy-read "Choose the action:"
                                `(("dir" . dired)
                                  ("vc-dir" . vc-dir)
                                  ("counsel-git" .
                                   ,(wrap-function-with-default-directory #'counsel-git))
                                  ("counsel-git-grep" .
                                   ,(wrap-function-with-default-directory #'counsel-git-grep))
                                  ("remove-customerized-dir" .
                                   (wrap-function-with-default-directory #'remove-customized-dir)))
                                :action (lambda (pair)
                                          (let ((f (cdr pair)))
                                            (funcall f dir)))))))

(defvar +customized-dir-file-name+ "~/.customized-dir-save")
(defvar +dired-al-mode-header+ "  drwx------.  0 user user     4096 Mar  0 00:00 ")
(defvar *customized-dir* nil)
(add-to-list 'auto-coding-alist '("\\.customized-dir-save\\'" . utf-8))

(defun load-customized-dir ()
  "."
  (interactive)
  (when (file-readable-p +customized-dir-file-name+)
    (with-temp-buffer
      (insert-file-contents +customized-dir-file-name+)
      (goto-char (point-min))
      (mapc #'(lambda (dir)
                (unless (member dir *customized-dir*)
                  (push dir *customized-dir*)))
            (read (current-buffer))))))

(defun save-customized-dir-without-confirm ()
  "."
  (with-temp-file +customized-dir-file-name+
    (print *customized-dir* (current-buffer))))

(defun save-customized-dir (confirmed)
  "CONFIRMED: ."
  (interactive (list (y-or-n-p (format "Sure to add %s to customized-dir? " default-directory))))
  (if confirmed
      (progn
        (save-customized-dir-without-confirm)
        (message "Save succeed!"))
    (message "Save canceled!")))

(defun customized-dired ()
  "."
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create "*Customized*"))
  (widen)
  (kill-all-local-variables)
  (setq buffer-read-only nil)
  (erase-buffer)
  (mapc (lambda (dir)
          (insert +dired-al-mode-header+ dir "\n"))
        *customized-dir*)
  (goto-char (point-min))
  (setq default-directory "/")
  (dired-mode default-directory "-al")
  (make-local-variable 'dired-sort-inhibit)
  (setq dired-sort-inhibit t)
  (set (make-local-variable 'dired-subdir-alist)
       (list (cons default-directory (point-min-marker))))
  (set (make-local-variable 'dired-subdir-switches) nil)
  (setq buffer-read-only nil)
  (insert "  " default-directory ":\n")
  (let ((point (point)))
    (insert "  " "wildcard dirs" "\n")
    (dired-insert-set-properties point (point)))
  (setq buffer-read-only t))

(defun add-customized-dir (confirm)
  "CONFIRM: ."
  (interactive (list (y-or-n-p (format "Sure to add %s to customized-dir? " default-directory))))
  (if confirm
      (progn
        (let ((dir (expand-file-name default-directory)))
          (unless (member dir *customized-dir*)
            (push dir *customized-dir*))
          (message "%s added." dir)))
    (message "Action canceled!")))

(defun remove-customized-dir (confirm)
  "CONFIRM."
  (interactive (list (y-or-n-p (format "Sure to remove %s to customized-dir? " default-directory))))
  (if confirm
      (let ((dir (expand-file-name default-directory)))
        (if (member dir *customized-dir*)
          (progn
            (setq *customized-dir* (delete dir *customized-dir*))
            (message "%s removed." dir))
          (message "%s is not a customized dir!" dir)))
    (message "Action canceled!")))

(defun customized-dir-init ()
  "."
  (add-hook 'kill-emacs-hook 'save-customized-dir-without-confirm)
  (load-customized-dir))


(require 'dired)
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map "V"
              '(lambda () (interactive)
                 (let ((file (dired-get-file-for-visit)))
                   (if (file-directory-p file)
                       (magit-status file)))))

            (define-key dired-mode-map "v"
              '(lambda () (interactive)
                 (let ((file (dired-get-file-for-visit)))
                   (if (file-directory-p file)
                       (vc-dir file)))))))

;; ace-jump-buffer
(require 'avy-config)
;; the <f4> is empty

(provide 'customized-dir)
;;; customized-dir.el ends here
