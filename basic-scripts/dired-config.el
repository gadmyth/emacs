;;; package --- dired-config.el
;;; Commentary:
;;; Code:


(eval-after-load 'dired
  '(progn
     ;; config some variables
     (setq dired-recursive-copies 'always)
     (setq dired-recursive-deletes 'always)
     (setq dired-dwim-target t)
     (setq dired-listing-switches "-lFaGh1v --group-directories-first -t")

     (when (eq system-type 'darwin)
       (require 'ls-lisp)
       (setq ls-lisp-use-insert-directory-program nil)
       (add-to-list 'dired-omit-extensions ".DS_Store"))

     ;; create file use \c keymap
     (defun dired-create-new-file (file)
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
           (dired-move-to-filename))))

     ;; add create file keymap
     (define-key dired-mode-map (kbd "c") 'dired-create-new-file)

     ;; require dirtree
     (require-safely 'dirtree)
     
     ;; require dired-x
     (require-package
      'dired-x
      ;; omit some files or directories under dired mode
      (setq-default dired-omit-files-p t))

     (require-safely
      'dired-narrow
      (define-key dired-mode-map (kbd "/") 'dired-narrow))))

(provide 'dired-config)
;;; dired-config.el ends here