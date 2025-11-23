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
       (require-package
        'dired-x
        (add-to-list 'dired-omit-extensions ".DS_Store")))

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

     (defun dired-create-directory-or-file ()
       "."
       (interactive)
       (let ((type (read-key "Make directory (d) or create file (f) ? ")))
         (pcase type
           (?d
            (call-interactively 'dired-create-directory))
           (?f
            (call-interactively 'dired-create-new-file))
           (_
            (message "wrong action type")))))

     (defvar dired-zip-compress-command-template "zip {} %o -r --filesync %i")
     
     (defvar dired-unzip-compress-command-template '("" "unzip {} -o -d %o %i"))

     (defun dired-compress-maybe-encrypt (inner-command)
       "Compress or uncompress maybe encrypt or decrypt under dired with INNER-COMMAND."
       (let* ((password (read-passwd "Please input password (default no password): "))
              (password-param (or (and (> (length password) 0)
                                       (format "-P \"%s\"" password))
                                  ""))
              (zip-command)
              (unzip-command))
         (ignore-errors
           (unwind-protect
               (progn
                 ;; Choose encrypt or not
                 (setq zip-command (replace-regexp-in-string
                                    "{}" password-param
                                    dired-zip-compress-command-template))
                 (setq unzip-command (list
                                      (car dired-unzip-compress-command-template)
                                      (replace-regexp-in-string
                                       "{}" password-param
                                       (cadr dired-unzip-compress-command-template))))
                 ;; (message "unzip command: %s" unzip-command)
                 ;; compose the new encrypt zip command
                 (setf (alist-get "\\.zip\\'" dired-compress-files-alist nil nil #'string-equal)
                       zip-command)
                 (setf (alist-get "\\.zip\\'" dired-compress-file-suffixes nil nil #'string-equal)
                       unzip-command)
                 ;; call the compress function
                 (call-interactively inner-command))
             ;; recover the original zip command
             (setf (alist-get "\\.zip\\'" dired-compress-files-alist nil nil #'string-equal)
                   (replace-regexp-in-string
                    "{}" ""
                    dired-zip-compress-command-template))
             (setf (alist-get "\\.zip\\'" dired-compress-file-suffixes nil nil #'string-equal)
                   (list
                    (car dired-unzip-compress-command-template)
                    (replace-regexp-in-string
                     "{}" "{}"
                     (cadr dired-unzip-compress-command-template))))))))

     ;; add create file keymap
     (define-key dired-mode-map (kbd "+") 'dired-create-directory-or-file)

     (define-key dired-mode-map (kbd "c")
       (lambda ()
         (interactive)
         (dired-compress-maybe-encrypt 'dired-do-compress-to)))

     (define-key dired-mode-map (kbd "Z")
       (lambda ()
         (interactive)
         (dired-compress-maybe-encrypt 'dired-do-compress)))

     (define-key dired-mode-map (kbd "C-k")
       (lambda ()
         (interactive)
         (if current-prefix-arg
             (when-let ((subdir (dired-get-subdir)))
               (dired-kill-tree subdir nil t)))))
     
     ;; require dirtree
     (require-package 'dirtree)
     
     ;; require dired-x
     (require-package
      'dired-x
      ;; omit some files or directories under dired mode
      (setq-default dired-omit-files-p t)
      ;; shell alist
      (setq dired-guess-shell-alist-user
            `(("\\.\\(jpe?g\\|png\\|gif\\)\\'"
               ,(cond ((eq window-system 'x) "ristretto" "viewnior")
                      (t nil)))
              ("\\.xlsx\\'"
               ,(cond ((eq window-system 'x) "gnumeric")
                      (t nil)))
              ("\\.htm?l\\'"
               ,(cond ((eq window-system 'x) "firefox" "google-chrome")
                      (t nil))))))

     (require-package
      'dired-narrow
      (define-key dired-mode-map (kbd "/") 'dired-narrow))))

(provide 'dired-config)
;;; dired-config.el ends here
