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

(let* ((init-files '(".emacs.frame.el" ".emacs.shell.el" ".emacs.simple.el"))
       (default-file ".emacs.frame.el")
       (selected (completing-read "Please select the init file:" init-files nil t nil nil default-file)))
  (load-file (expand-file-name selected +emacs-init-script-directory+)))

(provide 'emacs.init)
;;; emacs.init.el ends here

