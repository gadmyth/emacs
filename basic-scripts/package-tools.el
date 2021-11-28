;;; package --- package-tools.el
;;; Commentary:
;;; Code:

(require 'package)
(require 'find-func)

(defmacro require-safely (package &rest body)
  "PACKAGE, BODY."
  `(let ((should-require))
     (if (and (not should-require)
	          (not (package-installed-p ,package)))
	     (message "package [%S] is not installed!" ,package)
       (setq should-require t))
     (if (and (not should-require)
	          (not (ignore-errors (find-library-name (symbol-name ,package)))))
	     (message "package [%S] is not found in load-path!" ,package)
       (setq should-require t))
     (when should-require
       (require-package ,package ,@body))))

(defmacro require-package (package &rest body)
  "Require PACKAGE and execute the BODY."
  `(require-package-with-depends ,package nil ,@body))
  
(defmacro require-package-with-depends (package dependencies &rest body)
  "Require PACKAGE if all DEPENDENCIES are features and execute BODY."
  `(let ((all-package-featurep t))
     (dolist (p ,dependencies)
       (when (not (featurep p))
         (message "package [%S] is not a feature, can't install!" p ,package)
         (setq all-package-featurep nil)))
     (when all-package-featurep
       (require ,package)
       (message "package [%S] is required!" ,package)
       (progn ,@body))))

(defmacro require-packages-safely (packages &rest body)
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
  "PACKAGE is package name; MIN-VERSION is min version of package; NO-REFRESH is whether to refresh contents."
  (if (package-installed-p package min-version)
      t
    (progn
      (message "Should install-package: [%S]" package)
      (if (or (assoc package package-archive-contents) no-refresh)
          (package-install package)
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

(provide 'package-tools)
;;; package-tools.el ends here
