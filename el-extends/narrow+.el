;;; narrow+.el --- Package.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2024 Gadmyth

;; Author: narrow+.el <gadmyth@gmail.com>
;; Version: 1.0
;; Package-Version: 20241016.001
;; Package-Requires:
;; Keywords: narrow, region
;; Homepage: https://www.github.com/gadmyth/emacs
;; URL: https://www.github.com/gadmyth/emacs/blob/master/el-extends/narrow+.el

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
;; narrow+'s code can be found here:
;;   https://www.github.com/gadmyth/emacs/blob/master/el-extends/narrow+.el

;;; Commentary:
;;; Code:


(defun narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil t)))
    (with-current-buffer buf
      (narrow-to-region start end))
      (switch-to-buffer buf)))

(defun narrow-to-points (start-point end-point)
  "Narrow the buffer to the region between START-POINT and END-POINT."
  (interactive "r")
  (narrow-to-region start-point end-point))

(defun next-line-end-point (point)
  "Return the point at the end of the line following the line containing POINT."
  (save-excursion
    (goto-char point)
    (forward-line 1)
    (end-of-line)
    (point)))

(defun next-line-beginning-point (point)
  "Return the point at the end of the line following the line containing POINT."
  (save-excursion
    (goto-char point)
    (forward-line 1)
    (beginning-of-line)
    (point)))

(defun previous-line-beginning-point (point)
  "Return the point at the begining of the line following the line containing POINT."
  (save-excursion
    (goto-char point)
    (previous-line 1)
    (beginning-of-line)
    (point)))

(defun previous-line-end-point (point)
  "Return the point at the end of the line following the line containing POINT."
  (save-excursion
    (goto-char point)
    (previous-line 1)
    (end-of-line)
    (point)))
    
(defun enlarge-narrow-region-down ()
  (interactive)
  (let ((start (point-min))
        (end (point-max))
        (new-end)
        (current-indirect-buffer (current-buffer)))
    (with-current-buffer (buffer-base-buffer current-indirect-buffer)
      (setq new-end (next-line-end-point end)))
    (message "end: %S, new-end: %S" end new-end)
    (narrow-to-points start new-end)))

(defun enlarge-narrow-region-up ()
  (interactive)
  (let ((start (point-min))
        (end (point-max))
        (new-start)
        (current-indirect-buffer (current-buffer)))
    (with-current-buffer (buffer-base-buffer current-indirect-buffer)
      (setq new-start (previous-line-beginning-point start)))
    (message "start: %S, new-start: %S" start new-start)
    (narrow-to-points new-start end)))


(defun shrink-narrow-region-down ()
  (interactive)
  (let ((start (point-min))
        (end (point-max))
        (new-end)
        (current-indirect-buffer (current-buffer)))
    (with-current-buffer (buffer-base-buffer current-indirect-buffer)
      (setq new-end (previous-line-end-point end)))
    (message "end: %S, new-end: %S" end new-end)
    (narrow-to-points start new-end)))

(defun shrink-narrow-region-up ()
  (interactive)
  (let ((start (point-min))
        (end (point-max))
        (new-start)
        (current-indirect-buffer (current-buffer)))
    (with-current-buffer (buffer-base-buffer current-indirect-buffer)
      (setq new-start (next-line-beginning-point start)))
    (message "start: %S, new-start: %S" start new-start)
    (narrow-to-points new-start end)))

(defun adjust-narrow-region ()
  "."
  (interactive)
  (let* ((ev last-command-event)
         (base (event-basic-type ev)))
    (message "ev: %S, base: %S" ev base)
    (pcase base
      (?u (if (member 'shift (event-modifiers ev))
              ;; U: top boder, shrink
              (shrink-narrow-region-up)
            ;; u: top boder, enlarge
            (enlarge-narrow-region-up))
          (goto-char (point-min)))
      (?d (if (member 'shift (event-modifiers ev))
              ;; D: bottom border, enlarge
              (shrink-narrow-region-down)
            ;; d: bottom border, shrink
            (enlarge-narrow-region-down)
            (goto-char (point-max))))
      (_ nil)))
  (message "Use u to enlarge top border, U to shrink the bottom border, d to shrink top border, D to enlarge the bottom border.")
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (vector (list ?d))
                 (lambda () (interactive) (adjust-narrow-region)))
     (define-key map (vector (list ?D))
                 (lambda () (interactive) (adjust-narrow-region)))
     (define-key map (vector (list ?u))
                 (lambda () (interactive) (adjust-narrow-region)))
     (define-key map (vector (list ?U))
                 (lambda () (interactive) (adjust-narrow-region)))
     map)))

(global-set-key (kbd "C-c C-n r") #'narrow-to-region-indirect)
(global-set-key (kbd "C-c C-n u") #'adjust-narrow-region)
(global-set-key (kbd "C-c C-n d") #'adjust-narrow-region)

(provide 'narrow+)
;;; narrow+.el ends here
