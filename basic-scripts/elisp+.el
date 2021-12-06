;;; package --- elisp+.el
;;; Commentary:
;;; Code:

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (sp-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun eval-and-delete ()
  "Eval the preceding sexp, and delete the sexp."
  (interactive)
  (sp-kill-sexp)
  (condition-case nil
      (eval (read (current-kill 0)))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun eval-region-with-context (beg end)
  "."
  (interactive "r")
  (let* ((content (buffer-substring-no-properties beg end))
         (content (format "(%s)" content))
         (list (read content))
         (msg ""))
    (dolist (pair list)
      (let* ((key (car pair))
             (value-exp (cadr pair))
             (value (eval value-exp)))
        (setq msg (concat msg (format "%S: %S" key value) "\n"))))
    (message msg)))

(global-set-key (kbd "C-c e r") 'eval-and-replace)
(global-set-key (kbd "C-c e d") 'eval-and-delete)
(global-set-key (kbd "C-c e c") 'eval-region-with-context)

(provide 'elisp+)
;;; elisp+.el ends here
