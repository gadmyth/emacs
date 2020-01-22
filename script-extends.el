;;; package --- script-extends.el
;;; Commentary:
;;; Code:


(defconst +PRE-SCRIPT-DIR+ (expand-file-name "~/el-pre-scripts"))
(defconst +SCRIPT-EXTENDS-DIR+ (expand-file-name "~/el-extends"))


(defun load-pre-script-files ()
  "."
  (interactive)
  (load-script-files-in-directory +PRE-SCRIPT-DIR+))

(defun load-extend-script-files ()
  "."
  (interactive)
  (load-script-files-in-directory +SCRIPT-EXTENDS-DIR+))

(defun load-script-files-in-directory (directory)
  "DIRECTORY is the dir to load files."
  (interactive "Ddirectory: ")
  (mapc (lambda (script-file)
          (load-file script-file))
        (directory-files directory t ".*?\\.el$")))

(provide 'script-extends)
;;; script-extends.el ends here
