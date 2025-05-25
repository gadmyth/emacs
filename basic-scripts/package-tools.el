;;; package --- package-tools.el
;;; Commentary:
;;; Code:

(require 'package)
(require 'find-func)
(require 'q)

(defmacro require-package (package &rest args)
  "Require PACKAGE if all dependencies can be required, then execute body in ARGS."
  `(let ((could-require-p))
     ;; check package in load-path
     (unless could-require-p
       (if (not (ignore-errors (find-library-name (symbol-name ,package))))
	   (message "require-package [%S]: package is not found in load-path!" ,package)
         (setq could-require-p t)))
     ;; check package installed
     (unless could-require-p
       (unless (package-installed-p ,package)
	 (message "require-package [%S]: package is not installed, try to install..." ,package)
	 (when (y-or-n-p (format "Install %s or not? " ,package))
           (call-safely (package-install ,package)))
         ;; check package installed again
         (setq could-require-p (package-installed-p ,package))))
     ;; parse and require dependencies
     (when could-require-p
       (let* ((all-package-featurep t)
              ;; args is substitute as arg list, and will be eval, should quote it, don't eval it
              (arg-list ',args)
              ;; parse something from args, should eval it, because it quote before
              (dependencies (eval (plist-get arg-list :dependencies)))
              (body (if dependencies (seq-drop arg-list 2) arg-list)))
         ;; (message "arg-list: %S" arg-list)
         ;; (message "dependencies: %S" dependencies)
         ;; (message "body: %S" body)
         (dolist (p dependencies)
           (cond
            ;; aleady required
            ((featurep p)
             (message "require-package [%S]: dependent package [%S] is already required before!" ,package p))
            ;; find and require package in load-path
            ((ignore-errors (find-library-name (symbol-name p)))
             (require p)
             (message "require-package [%S]: dependent package [%S] is required!" ,package p))
            ;; package is already installed
            ((package-installed-p p)
             (require p)
             (message "require-package [%S]: dependent package [%S] is required!" ,package p))
            ;; try to install package
            (t
             (message "require-package [%S]: try to install package: %S..." ,package p)
             (call-safely
              (package-install p)
              (setq all-package-featurep nil))
             (cond
              ;; require installed package
              ((package-installed-p p)
               (require p)
               (message "require-package [%S]: dependent package [%S] is required!" ,package p))
              (t
               ;; can't require an uninstalled package
               (message "require-package [%S]: dependent package [%S] is not installed or is not a feature, can't install %S!" ,package p ,package)
               (setq all-package-featurep nil))))))
         (when all-package-featurep
           (cond
            ;; already required
            ((featurep ,package)
             (message "require-package [%S]: package is already required before!" ,package))
            (t
             (require ,package)
             (message "require-package [%S]: package is required!" ,package)))
           (when body
             ;; body is a list not eval, should eval each one in the body list
             (seq-doseq (expression body)
               ;; (message "exp: %S" expression)
               (eval expression))))))
     nil))

(defmacro require-packages (packages &rest body)
  "PACKAGES, BODY."
  `(let ((all-package-installed t))
     (dolist (p ,packages)
       (when (not (package-installed-p p))
         (message "package [%S] is not installed!" p)
         (setq all-package-installed nil)))
     (when all-package-installed
       (dolist (p ,packages)
         (require-package p))
       ,@body)))

(defun require-library (library)
  "Require the Emacs Lisp source of LIBRARY."
  (interactive (list (read-library-name)))
  (require-package (read library)))

(defvar *sync-package* t)

(defun install-package (package &optional min-version no-refresh)
  "Install PACKAGE with MIN-VERSION, if NO-REFRESH is t, install directly."
  (if (package-installed-p package min-version)
      t
    (progn
      (message "Should install-package: [%S]" package)
      (if (or (assoc package package-archive-contents) no-refresh)
          (call-safely (package-install package))
        (progn
          (package-refresh-contents)
          (install-package package min-version t))))))

(defun install-packages-if-needed (package-list)
  "Install packages if not existed, PACKAGE-LIST."
  (message "install-packages-if-needed: [%S]" *sync-package*)
  (if *sync-package*
      (mapcar #'install-package package-list)))

(defun set-pair-to-alist (list key value)
  "Set KEY, VALUE pair to LIST."
  (let ((old-value (cdr (assoc key (symbol-value list))))
        (new-value value))
    (when (or (not old-value)
              (not (equal old-value new-value)))
      (setf (alist-get key (symbol-value list) nil t 'equal) new-value))))

(defun packages-init-archives ()
  "."
  (when (bound-and-true-p *customize-package-archives*)
    (seq-doseq (archive *customize-package-archives*)
      (set-pair-to-alist 'package-archives (car archive) (cdr archive)))))

(provide 'package-tools)
;;; package-tools.el ends here
