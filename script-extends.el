;;; package --- script-extends.el
;;; Commentary:
;;; Code:


(defconst +PRE-SCRIPT-DIR+ (expand-file-name "el-pre-scripts" +emacs-context-directory+))
(defconst +SCRIPT-EXTENDS-DIR+ (expand-file-name "el-extends" +emacs-context-directory+))
(defconst +POST-SCRIPT-DIR+ (expand-file-name "el-post-scripts" +emacs-context-directory+))


(defun load-pre-script-files ()
  "."
  (interactive)
  (load-script-files +PRE-SCRIPT-DIR+))

(defun load-extend-script-files ()
  "."
  (interactive)
  (load-script-files +SCRIPT-EXTENDS-DIR+))

(defun load-post-script-files ()
  "."
  (interactive)
  (load-script-files +POST-SCRIPT-DIR+))

(defun load-script-files (dir)
  "Load the script files in the DIR."
  (load-script-files-in-directory dir))

(defun load-script-files-in-directory (directory)
  "DIRECTORY is the dir to load files."
  (interactive "Ddirectory: ")
  (mapc (lambda (script-file)
          (load-file script-file))
        (directory-files directory t ".*?\\.el$")))

(provide 'script-extends)
;;; script-extends.el ends here
