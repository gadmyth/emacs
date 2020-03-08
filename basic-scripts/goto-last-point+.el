;;; package --- goto-last-point+.el
;;; Commentary:
;;; Code:

(require 'goto-last-point)

(defcustom goto-next-point-max-length
  5
  "Maximum length of the redo stack."
  :group 'goto-last-point
  :type 'integer)

(defvar goto-last-point-pre-hook nil
  "Hook called after a jump happens.")

(defvar-local goto-next-point-stack nil
  "The point redo stack.")

(defvar *goto-last-point-debug* nil
  "If not nil, show debug message.")

(defun goto-last-point ()
  "Copied and modified from goto-last-point.el."
  (interactive)
  (ensure-last-point-stack)
  (when (not (ring-empty-p goto-last-point-stack))
    (let ((last-place (ring-remove goto-last-point-stack 0))
          (current-point (point)))
      (when last-place (goto-char last-place))
      (run-hooks 'goto-last-point-goto-hook)
      (if (= current-point last-place)
          (goto-last-point)))))

(defun goto-next-point ()
  "Jump to the next point."
  (interactive)
  (ensure-next-point-stack)
  (when (not (ring-empty-p goto-next-point-stack))
    (let ((next-point (ring-remove goto-next-point-stack 0))
          (current-point (point)))
      (when next-point (goto-char next-point)
            (ensure-last-point-stack)
            (ring-insert goto-last-point-stack next-point))
      (if (= current-point next-point)
          (goto-next-point)))))

(defun goto-next-point--record ()
  "Collect POINT into goto-next-point-stack."
  (ensure-next-point-stack)
  (ring-insert goto-next-point-stack (point)))

(defun goto-last-point--record ()
  "Copied and modified from goto-last-point.el."
  (unless (or (minibufferp)
              (not (eq this-command 'record-current-point)))
    (ensure-last-point-stack)
    (when (or (ring-empty-p goto-last-point-stack)
              (/= (ring-ref goto-last-point-stack 0) (point)))
      (ring-insert goto-last-point-stack (point)))))

(defun ensure-next-point-stack ()
  "Ensure the next-point-stack is a local variable."
  (unless (and (local-variable-p 'goto-next-point-stack)
               goto-next-point-stack)
    (set (make-local-variable 'goto-next-point-stack)
         (make-ring goto-next-point-max-length))))

(defun ensure-last-point-stack ()
  "Ensure the next-point-stack is a local variable."
  (unless (and (local-variable-p 'goto-last-point-stack)
               goto-last-point-stack)
    (set (make-local-variable 'goto-last-point-stack)
         (make-ring goto-last-point-max-length))))

(defun goto-last-point--clear (_ _1 _2)
  "Copied from goto-last-point.el."
  (progn
    (setq goto-last-point-stack nil)
    (setq goto-next-point-stack nil)))

(defun record-current-point ()
  "."
  (interactive))

;; debug function to print to this command
(defun print-command-name ()
  "."
  (when *goto-last-point-debug*
    ;(message "this command: %s" this-command)
    (message "last-point-stack: %S" goto-last-point-stack)
    (message "next-point-stack: %S" goto-next-point-stack)))

;; add hooks
(add-hook 'post-command-hook #'print-command-name)
(add-hook 'goto-last-point-goto-hook #'record-next-point--record)

;; define the global key
(global-set-key (kbd "<f9>") 'goto-last-point)
(global-set-key (kbd "<f10>") 'goto-next-point)
(global-set-key (kbd "<f11>") 'record-current-point)

;; turn the mode on
(goto-last-point-mode)

(provide 'goto-last-point+)
;;; goto-last-point+.el ends here
