;;; package --- emacs.init.el
;;; Commentary:
;;; Code:


(defun emacs-load-path-initialize ()
  "."
  (dolist (path '("basic-scripts"
                  "el-extends"
                  "el-pre-scripts"
                  "el-post-scripts"))
    (add-to-list 'load-path (expand-file-name path +emacs-context-directory+))))

(load-file (expand-file-name ".emacs.frame.el" +emacs-init-script-directory+))

(provide 'emacs.init)
;;; emacs.init.el ends here

