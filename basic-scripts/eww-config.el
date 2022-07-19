;;; package --- eww-config.el
;;; Commentary:
;;; Code:

(require 'shr)

(defun toggle-browser-function ()
  "."
  (interactive)
  (if (equal browse-url-browser-function 'eww-browse-url)
      (setq browse-url-browser-function 'browse-url-default-browser)
    (setq browse-url-browser-function 'eww-browse-url))
  (message "Current browser function is: %S" browse-url-browser-function))

(defvar *shr-fill-functions-backup-p* nil)

(defun shr-fill-text-original (text)
  "Original function for sh-fill-text with TEXT parameter.")

(defun shr-fill-lines-original (start end)
  "Original function for sh-fill-lines with START and END parameters.")

(defun shr-fill-line-original ()
  "Original function for sh-fill-line.")

(defun shr-fill-text-no-fill (text) text)

(defun shr-fill-lines-no-fill (start end) nil)

(defun shr-fill-line-no-fill () nil)

(defun shr-fill-backup ()
  (unless *shr-fill-functions-backup-p*
    (setf (symbol-function 'shr-fill-text-original)
          (symbol-function 'shr-fill-text))
    (setf (symbol-function 'shr-fill-lines-original)
          (symbol-function 'shr-fill-lines))
    (setf (symbol-function 'shr-fill-line-original)
          (symbol-function 'shr-fill-line))
    (setq *shr-fill-functions-backup-p* t)))

(defun shr-fill-swap ()
  (setf (symbol-function 'shr-fill-text)
        (symbol-function 'shr-fill-text-no-fill))
  (setf (symbol-function 'shr-fill-lines)
        (symbol-function 'shr-fill-lines-no-fill))
  (setf (symbol-function 'shr-fill-line)
        (symbol-function 'shr-fill-line-no-fill)))

(defun shr-fill-restore ()
  (setf (symbol-function 'shr-fill-text)
        (symbol-function 'shr-fill-text-original))
  (setf (symbol-function 'shr-fill-lines)
        (symbol-function 'shr-fill-lines-orignal))
  (setf (symbol-function 'shr-fill-line)
        (symbol-function 'shr-fill-line-original)))

(define-minor-mode shr-no-fill-mode
  "Global minor mode which prevents `shr' and `eww' from filling text output."
  ;; :lighter (:eval (if (derived-mode-p 'eww-mode) " ShrNoFill"))
  :global t
  (if shr-no-fill-mode
      (progn
        (shr-fill-backup)
        (shr-fill-swap))
    (shr-fill-restore)))

;(shr-no-fill-mode 1)

(provide 'eww-config)
;;; eww-config.el ends here
