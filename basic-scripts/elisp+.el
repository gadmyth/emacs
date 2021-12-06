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
  "Eval the region of the let pairs from BEG to END."
  (interactive "r")
  (let* ((content (buffer-substring-no-properties beg end))
         (content (format "(%s)" content))
         (list (read content))
         (msg "The eval result:"))
    (eval (generate-let-context
           list
           (lambda (content)
             (setq msg (format "%s\n%s" msg content)))))
    (message "%s" msg)))

(defun generate-let-context (list callback)
  "Generate let context with contexts of LIST, call CALLBACK every level of let body."
  (if (= (length list) 0)
      nil
    (let* ((pair (car list))
           (key (car pair))
           (value-exp (cadr pair)))
      `(let (,pair)
         (funcall ,callback (format "%s: %s" ',key ,value-exp))
         ,(generate-let-context (cdr list) callback)))))

(global-set-key (kbd "C-c e r") 'eval-and-replace)
(global-set-key (kbd "C-c e d") 'eval-and-delete)
(global-set-key (kbd "C-c e c") 'eval-region-with-context)

(provide 'elisp+)
;;; elisp+.el ends here
