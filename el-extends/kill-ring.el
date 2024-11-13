;;; package --- kill-ring.el
;;; Commentary:
;;; Code:

(require 'q)

(define-debug-message kill-ring)

;; ------ line editor -------

(defun kill-to-beginning-of-line ()
  "."
  (interactive)
  (save-excursion
    (let ((end (point))
          (begin (progn
                   (beginning-of-visual-line)
                   (point))))
    (kill-region begin end))))

(defun kill-the-whole-line ()
  "."
  (interactive)
  (save-excursion
    (let ((begin (progn
                   (beginning-of-visual-line)
                   (point)))
          (end (progn
                 (end-of-visual-line)
                 (point))))
      (kill-region begin end))))

(defun kill-the-whole-line-ring-save ()
  "START, END."
  (interactive)
  (let* ((region-active-p (region-active-p))
         (text-beg (if region-active-p (region-beginning) (line-beginning-position)))
         (text-end (if region-active-p (region-end) (line-end-position)))
         (content (buffer-substring-no-properties text-beg text-end)))
    (try-kill-to-system-clipboard content)
    (cond
     (region-active-p
      (kill-ring-debug-message "*** region copied ***")
      (deactivate-mark))
     (t
      (kill-ring-debug-message "*** line copied ***")))))

(defun mark-the-whole-line ()
  "."
  (interactive)
  (beginning-of-visual-line)
  (push-mark-command nil)
  (end-of-visual-line))

(defun remove-all-text-properties-region (beg end)
  "Remove all the text properties with region from BEG to END."
  (interactive "r")
  (when (region-active-p)
    (set-text-properties beg end nil)))

;; ----- global keys for line editor -----

;; C-k: kill-line from current point to end of line
;; C-S-u: kill-to-beginning-of-line
;; C-S-k: kill the whole line
;; M-w origin key bind to kill-ring-save, original function is copy region

(global-set-key (kbd "C-S-u") 'kill-to-beginning-of-line)
(global-set-key (kbd "C-S-k") 'kill-the-whole-line)
(global-set-key (kbd "M-w") 'kill-the-whole-line-ring-save)
(global-set-key (kbd "C-S-l") 'mark-the-whole-line)

(provide 'kill-ring)
;;; kill-ring.el ends here
