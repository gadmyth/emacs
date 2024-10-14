;;; package --- files+.el
;;; Commentary:
;;; Code:


(defmacro find-file-path (root-directory file-part-name file-hint &optional initial &rest body)
  `(let* ((file-path)
          (cmd (format "find %s -name '%s*' | xargs realpath" ,root-directory ,file-part-name))
          (files (shell-command-to-string cmd))
          (files (string-split files))
          (filtered-files (if (not ,initial)
                            (cl-remove-if-not
                             (lambda (file) (string-match-p (regexp-quote ,initial) file))
                             files))))
     (when-let ((file-path
                 (if (= (length filtered-files) 1)
                     (car filtered-files)
                   (completing-read (format "Please select %s: " ,file-hint) files nil t ,initial))))
       ,@body)))

(defmacro find-project-file-path (file-part-name file-hint &optional initial &rest body)
  `(when-let ((root-directory (textmate-find-project-root)))
     (find-file-path root-directory ,file-part-name ,file-hint ,initial ,@body)))

(defun find-project-file-at-point ()
  (interactive)
  (require-package 'thingatpt+)
  (when-let ((word (region-or-word-at-point)))
    (require-package 'textmate)
    (when-let ((root-directory (textmate-find-project-root)))
      (find-file-path root-directory word "file" nil
                      (find-file file-path)))))

(provide 'files+)
;;; files+.el ends here
