;;; package --- dired++.el
;;; Commentary:
;;; Code:


(require 'dired)

(add-hook 'after-init-hook (lambda () (require-if-installed 'dirtree)))

(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq dired-dwim-target t)

(eval-after-load 'dired
  '(progn
     (define-key dired-mode-map (kbd "c") 'dired-create-file2)
     (defun dired-create-file2 (file)
       "Create a file called FILE. If FILE already exists, signal an error."
       (interactive
        (list (read-file-name "Create file: " (dired-current-directory))))
       (let* ((expanded (expand-file-name file))
              (try expanded)
              (dir (directory-file-name (file-name-directory expanded)))
              new)
         (if (file-exists-p expanded)
             (error "Cannot create file %s: file exists" expanded))
         ;; Find the topmost nonexistent parent dir (variable `new')
         (while (and try (not (file-exists-p try)) (not (equal new try)))
           (setq new try
                 try (directory-file-name (file-name-directory try))))
         (when (not (file-exists-p dir))
           (make-directory dir t))
         (write-region "" nil expanded t)
         (when new
           (dired-add-file new)
           (dired-move-to-filename))))))


(provide 'dired++)

;;; dired++.el ends here