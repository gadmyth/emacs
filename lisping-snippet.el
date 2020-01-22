;;; package --- lisping-snippet.el
;;; Commentary:
;;; Code:

(require 'yasnippet)

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

(defun expand-template (template-key)
  "TEMPLATE-KEY: ."
  (interactive "syasnippet's template: ")
  (yas--expand-or-prompt-for-template
   (cl-mapcan (lambda (table)
                (yas--fetch table template-key))
              (yas--get-snippet-tables))))


(global-set-key (kbd "C-c e r") 'eval-and-replace)
(global-set-key (kbd "C-c e d") 'eval-and-delete)


(provide 'lisping-snippet)
;;; lisping-snippet.el ends here