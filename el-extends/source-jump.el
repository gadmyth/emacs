;;; package --- source-jump.el
;;; Commentary:
;;; Code:


(defun sj-save-excursion-p ()
  "."
  nil)

(defmacro sj-save-excursion (&rest body)
  "BODY."
  `(if (sj-save-excursion-p)
       (save-excursion
         (progn ,@body))
     (progn ,@body)))

(defun sj-action-with-regexp (regexp prompt empty-message select-action &optional buffer)
  "REGEXP, PROMPT, EMPTY-MESSAGE, SELECT-ACTION, BUFFER."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (let (collections '())
      (sj-save-excursion
       (goto-char (point-min))
       (while (re-search-forward regexp nil t)
         (push (list (match-string 0) (line-number-at-pos (point))) collections)))
      (if (> (length collections) 0)
          (funcall select-action collections)
        (message empty-message)))))

(defun sj-goto-line-no-interactive (line-num)
  "LINE-NUM."
  (goto-char (point-min))
  (forward-line (1- line-num)))

(defun sj-goto-with-regexp (regexp prompt empty-message)
  "REGEXP, PROMPT, EMPTY-MESSAGE."
  (sj-action-with-regexp regexp prompt empty-message
                           #'(lambda (collections)
                               (ivy-read prompt (reverse collections) :action
                                         (lambda (candidate)
                                           (let ((line-num (cadr candidate)))
                                             (sj-goto-line-no-interactive line-num)))))))

(defun sj-goto-last-with-regexp (regexp prompt empty-message &optional finish-block)
  "REGEXP, PROMPT, EMPTY-MESSAGE, FINISH-BLOCK."
  (sj-action-with-regexp regexp prompt empty-message
                           #'(lambda (collections)
                               (let ((content (caar (last (reverse collections)))))
                                 (message content)
                                 (if finish-block (funcall finish-block))))))

(defun sj-show-last-with-regexp (regexp prompt empty-message &optional finish-block)
  "REGEXP, PROMPT, EMPTY-MESSAGE, FINISH-BLOCK."
  (sj-action-with-regexp regexp prompt empty-message
                         #'(lambda (collections)
                             (let ((line-num (cadar (last (reverse collections)))))
                               (sj-goto-line-no-interactive line-num)
                               (message (thing-at-point 'line))
                               (if finish-block (funcall finish-block))))))

(defun sj-goto-line-or-select (prompt collections)
  "PROMPT, COLLECTIONS."
  (if (= (length collections) 1)
      (sj-goto-line-no-interactive (cadar collections))
    (ivy-read prompt (reverse collections) :action
              (lambda (candidate)
                (let ((line-num (cadr candidate)))
                  (sj-goto-line-no-interactive line-num))))))

(defun yas-expand-snippet-with-params (snippet-name &rest params)
  "SNIPPET-NAME, PARAMS."
  (interactive)
  (when-let ((snippet (yas-lookup-snippet snippet-name)))
    (yas-expand-snippet snippet)
    (dolist (p params)
      (if (or (string-equal "__default__" p)
              (string-equal "" p))
          (yas-next-field)
        (progn
          (insert p)
          (yas-next-field))))))

(defmacro lambda-of-ivy-read (&rest body)
  "COLLECTIONS, BODY."
  `(lambda (collections)
      (ivy-read "The methods: " (reverse collections) :action
                #'(lambda (candidate)
                    ,@body))))

(provide 'source-jump)
;;; source-jump.el ends here