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
    (let* ((type (completing-read "Please select the value type: " '(string number bool)))
           (value
            (pcase type
              ("string" (read-string (format "Please input value for [%s]: " (symbol-name sym))))
              ("number" (read-number (format "Please input value for [%s]: " (symbol-name sym))))
          ("bool" (y-or-n-p (format "Please input value for [%s]: " (symbol-name sym))))
              (_ (message (format "not support value type: %S" type))))))
      (when (or value (string-equal type "bool"))
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

(provide 'at-point)
;;; anythings.el ends here
