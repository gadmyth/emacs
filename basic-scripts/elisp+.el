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

(global-set-key (kbd "C-c e r") 'eval-and-replace)
(global-set-key (kbd "C-c e d") 'eval-and-delete)

(provide 'elisp+)
;;; elisp+.el ends here
