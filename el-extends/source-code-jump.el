;;; package --- source-code-jump.el
;;; Commentary:
;;; Code:

(require 'yas-config)

(defun scj-save-excursion-p ()
  "."
  nil)

(defmacro scj-save-excursion (&rest body)
  "BODY."
  `(if (scj-save-excursion-p)
       (save-excursion
         (progn ,@body))
     (progn ,@body)))

(defun scj-action-with-regexp (regexp prompt empty-message select-action &optional buffer)
  "REGEXP, PROMPT, EMPTY-MESSAGE, SELECT-ACTION, BUFFER."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (let (collections '())
      (scj-save-excursion
       (goto-char (point-min))
       (while (re-search-forward regexp nil t)
         (push (list (match-string 0) (line-number-at-pos (point))) collections)))
      (if (> (length collections) 0)
          (funcall select-action collections)
        (message empty-message)))))

(defun scj-goto-line-no-interactive (line-num)
  "LINE-NUM."
  (goto-char (point-min))
  (forward-line (1- line-num)))

(defun scj-goto-with-regexp (regexp prompt empty-message)
  "REGEXP, PROMPT, EMPTY-MESSAGE."
  (scj-action-with-regexp regexp prompt empty-message
                           #'(lambda (collections)
                               (ivy-read prompt (reverse collections) :action
                                         (lambda (candidate)
                                           (let ((line-num (cadr candidate)))
                                             (scj-goto-line-no-interactive line-num)))))))

(defun scj-collect-with-regexp (regexp prompt empty-message)
  "REGEXP, PROMPT, EMPTY-MESSAGE."
  (scj-action-with-regexp regexp prompt empty-message
                          #'(lambda (collections)
                              (let ((list (mapcar (lambda (rich)
                                                    (let* ((content (car rich))
                                                           (start  0)
                                                           (end (length rich)))
                                                      (substring-no-properties content)))
                                                  collections)))
                                (reverse list)))))

(defun scj-goto-last-with-regexp (regexp prompt empty-message &optional finish-block)
  "REGEXP, PROMPT, EMPTY-MESSAGE, FINISH-BLOCK."
  (scj-action-with-regexp regexp prompt empty-message
                           #'(lambda (collections)
                               (let ((content (caar (last (reverse collections)))))
                                 (message content)
                                 (if finish-block (funcall finish-block))))))

(defun scj-show-last-with-regexp (regexp prompt empty-message &optional finish-block)
  "REGEXP, PROMPT, EMPTY-MESSAGE, FINISH-BLOCK."
  (scj-action-with-regexp regexp prompt empty-message
                         #'(lambda (collections)
                             (let ((line-num (cadar (last (reverse collections)))))
                               (scj-goto-line-no-interactive line-num)
                               (message (thing-at-point 'line))
                               (if finish-block (funcall finish-block))))))

(defun scj-goto-line-or-select (prompt collections)
  "PROMPT, COLLECTIONS."
  (if (= (length collections) 1)
      (scj-goto-line-no-interactive (cadar collections))
    (ivy-read prompt (reverse collections) :action
              (lambda (candidate)
                (let ((line-num (cadr candidate)))
                  (scj-goto-line-no-interactive line-num))))))

(defmacro lambda-of-ivy-read (&rest body)
  "COLLECTIONS, BODY."
  `(lambda (collections)
      (ivy-read "The methods: " (reverse collections) :action
                #'(lambda (candidate)
                    ,@body))))

(provide 'source-code-jump)
;;; source-code-jump.el ends here
