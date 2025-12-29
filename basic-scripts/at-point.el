;;; package --- at-point.el
;;; Commentary:
;;; Code:


(defun symbol-at-region-or-at-point ()
  "."
  (let* ((sym (and (region-active-p)
                   (let ((region-string (buffer-substring-no-properties (region-beginning) (region-end))))
                     (when (> (length region-string) 0)
                       (intern region-string)))))
         (sym (or sym (symbol-at-point)))
         (sym (or sym (let ((symbol-string (read-string "Please input the symbol: ")))
                        (when (> (length symbol-string) 0)
                          (intern symbol-string))))))
    sym))

(defun show-symbol-at-point ()
  "."
  (interactive)
  (let* ((sym (symbol-at-region-or-at-point)))
    (message "Symbol: [%S] is function: [%S], value: [%S]"
             sym
             (fboundp sym)
             (cond ((boundp sym)
                    (symbol-value sym))
                   (t
                    :_unbounded_)))))

(defun set-symbol-value-at-point ()
  "."
  (interactive)
  (let* ((sym (symbol-at-region-or-at-point)))
    (let* ((type (completing-read "Please select the value type: " '(string number bool symbol value)))
           (value
            (pcase type
              ("string" (read-string (format "Please input string value for [%s]: " (symbol-name sym))))
              ("number" (read-number (format "Please input number value for [%s]: " (symbol-name sym))))
              ("bool" (y-or-n-p (format "Please input bool value for [%s]: " (symbol-name sym))))
              ("symbol" (intern (read-string (format "Please input symbol for [%s]: " (symbol-name sym)))))
              ("value" (eval (car (read-from-string (read-string (format "Please input the expression for [%s]: " (symbol-name sym)))))))
              (_ (message (format "not support value type: %S" type))))))
      (when (or value (or (string-equal type "bool")
                          (string-equal type "value")))
        (setf (symbol-value sym) value)))))

(defun execute-function-at-point ()
  "."
  (interactive)
  (let ((sym (symbol-at-region-or-at-point)))
    (cond ((fboundp sym)
           (message "Now execute function [%S]" sym)
           (funcall (symbol-function sym)))
          (t
           (message "Symbol [%S] is not a function")))))

(defun find-library-at-point ()
  "."
  (interactive)
  (find-library (word-at-point)))

(defun open-image-at-point ()
  "."
  (interactive)
  (let ((path (thing-at-point-file-at-point))
        (program "viewnior"))
    (when (and (> (length path) 0)
               (executable-find program))
      (start-process program nil program path))))

(defun isearch-forward-region-or-symbol-at-point (&optional arg)
  "Copied from isearch-forward-symbol-at-point, support isearch region text."
  (interactive "P")
  (cond
   ((region-active-p)
    (isearch-forward nil 1)
    (let ((beg (region-beginning))
          (end (region-end)))
      (deactivate-mark)
      (when (< beg (point))
	    (goto-char beg))
      (isearch-yank-string
       (buffer-substring-no-properties beg end))
      (let ((count (and arg (prefix-numeric-value arg))))
        (when count
          (isearch-repeat-forward count)))
      ))
   (t
    (isearch-forward-symbol-at-point arg))))

(global-set-key (kbd "M-s .") #'isearch-forward-region-or-symbol-at-point)

(defun confirm-to-downcase-region ()
  "Call the downcase-region, but when region is not active, it won't downcase the text between
point and the mark is operated on like downcase-region."
  (interactive)
  (cond
   ((and (region-active-p)
         (y-or-n-p "Do you want to downcase the region? "))
    (downcase-region (region-beginning) (region-end)))
   (t (message "You should activate a region first, then downcase the region"))))

(global-set-key (kbd "C-x C-l") #'confirm-to-downcase-region)

(defun confirm-to-upcase-region ()
  "Call the upcase-region, but when region is not active, it won't upcase the text between
point and the mark is operated on like upcase-region."
  (interactive)
  (cond
   ((and (region-active-p)
         (y-or-n-p "Do you want to upcase the region? "))
    (upcase-region (region-beginning) (region-end)))
   (t (message "You should activate a region first, then upcase the region"))))

(global-set-key (kbd "C-x C-u") #'confirm-to-upcase-region)

(provide 'at-point)
;;; anythings.el ends here
