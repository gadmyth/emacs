;;; package --- my init file
;;; Commentary:

;;; Code:
(defvar *load-config-from-initial-file*)
(setq *load-config-from-initial-file* t)

;; current file is <parent dir>/emacs.git/init-script/.emacs,
;; +emacs-init-script-directory+ should be <parent dir>/emacs.git/init-script
;; +emacs-context-directory+ should be <parent dir>/emacs.git/
(defconst +emacs-init-script-directory+
  (let* ((file-name (or load-file-name buffer-file-name))
         (true-name (file-truename file-name))
         (directory (file-name-directory true-name)))
    directory))

(defconst +emacs-context-directory+
  (file-name-directory (directory-file-name +emacs-init-script-directory+)))

(when *load-config-from-initial-file*
  (let ((init-file-name (expand-file-name "emacs.init.el" +emacs-init-script-directory+)))
    (when (file-exists-p init-file-name)
      (load-file init-file-name))))

;;; add (package-initialize) here to satisfy contains-init in package.el.gz
;;; .emacs ends here
