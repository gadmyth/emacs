;;; package --- package-tools.el
;;; Commentary:
;;; Code:

(require 'package)

(defmacro require-safely (package &rest body)
  "PACKAGE, BODY."
  `(if (package-installed-p ,package)
       (require-package ,package ,@body)
     (message "%S not installed!" ,package)))

(defmacro require-package (package &rest body)
  "Require PACKAGE and execute the BODY."
  `(require-package-with-depends ,package nil ,@body))
  
(defmacro require-package-with-depends (package dependencies &rest body)
  "Require PACKAGE if all DEPENDENCIES are features and execute BODY."
  `(let ((all-package-featurep t))
     (dolist (p ,dependencies)
       (when (not (featurep p))
         (message "%S is not a feature, can't install package %S!" p ,package)
         (setq all-package-featurep nil)))
     (when all-package-featurep
       (require ,package)
       (message "%S required!" ,package)
       (progn ,@body))))

(defmacro require-packages-safely (packages &rest body)
  "PACKAGES, BODY."
  `(let ((all-package-installed t))
     (dolist (p ,packages)
       (when (not (package-installed-p p))
         (message "%S not installed!" p)
         (setq all-package-installed nil)))
     (when all-package-installed
       (dolist (p ,packages)
         (require-package p ,@body)))))

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
      (message "Should install-package: %S" package)
      (if (or (assoc package package-archive-contents) no-refresh)
          (package-install package)
        (progn
          (package-refresh-contents)
          (install-package package min-version t))))))

(defun install-packages-if-needed (package-list)
  "Install packages if not existed, PACKAGE-LIST."
  (message "install-packages-if-needed: %S" *sync-package*)
  (if *sync-package*
      (mapcar #'install-package package-list)))

(provide 'package-tools)
;;; package-tools.el ends here
