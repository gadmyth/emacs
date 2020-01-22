;;; package --- work-temp.el
;;; Commentary:
;;; Code:


(defun load-snippets (path)
  "PATH: ."
  (interactive "dLoad snippet directory: ")
  (yas-load-directory (expand-file-name path)))

(defun load-credit-snippets ()
  "."
  (interactive)
  (load-snippets "~/credit_APP/snippets"))

(defun load-emacs-snippets ()
  "."
  (interactive)
  (load-snippets "~/emacs/snippets"))

(defun update-upgrade ()
  "."
  (interactive)
  (export-org-html
   "upgrade"
   "upgrade.org"
   (expand-file-name "~/org/doc/upgrade.html")
   (expand-file-name "~/upgrade/upgrade.html")))

(defun update-jpush ()
  "."
  (interactive)
  (export-org-html
   "jpush-intro"
   "jpush.org"
   (expand-file-name "~/org/doc/jpush.html")
   (expand-file-name "~/jpush/jpush.html")))

(defun update-gps ()
  "."
  (interactive)
  (export-org-html
   "gps-permission"
   "gps.org"
   (expand-file-name "~/org/doc/gps.html")
   (expand-file-name "~/gps/gps.html")))

(defun update-ar ()
  "."
  (interactive)
  (export-org-html
   "wdcredit-AR"
   "ar.org"
   (expand-file-name "~/org/doc/ar.html")
   (expand-file-name "~/ar/ar.html")))

(defun export-org-html (subtree-id output-buffer-name src-html dest-html)
  "SUBTREE-ID, OUTPUT-BUFFER-NAME, SRC-HTML, DEST-HTML."
  (interactive)
  (with-current-buffer output-buffer-name
    (erase-buffer))
  (org-id-goto subtree-id)
  (org-copy-subtree)
  (with-current-buffer output-buffer-name
    (org-paste-subtree)
    (save-buffer)
    (org-html-export-to-html))
  (copy-file src-html dest-html  t nil t))

(defun goto-credit-objc-snippets ()
  "."
  (interactive)
  (dired "~/credit_APP/snippets/objc-mode/"))

(defun goto-emacs-objc-snippets ()
  "."
  (interactive)
  (dired "~/emacs/snippets/objc-mode/"))

(provide 'work-temp)
;;; work-temp.el ends here
