;;; package --- gkroam-config.el
;;; Commentary:
;;; Code:

(require-packages-safely
 '(gkroam rg)

 (let ((dir *gkroam-default-root-dir*))
   (when (and (file-exists-p dir)
              (f-directory-p dir))
     (setq gkroam-root-dir dir)))

 (setq gkroam-prettify-p t
       gkroam-show-brackets-p t
       gkroam-use-default-filename t
       gkroam-window-margin 4)

 (define-key gkroam-mode-map (kbd "C-c k I") #'gkroam-index)
 (define-key gkroam-mode-map (kbd "C-c k d") #'gkroam-daily)
 (define-key gkroam-mode-map (kbd "C-c k f") #'gkroam-find)
 (define-key gkroam-mode-map (kbd "C-c k i") #'gkroam-insert)
 (define-key gkroam-mode-map (kbd "C-c k c") #'gkroam-capture)
 (define-key gkroam-mode-map (kbd "C-c k e") #'gkroam-link-edit)
 (define-key gkroam-mode-map (kbd "C-c k n") #'gkroam-smart-new)
 (define-key gkroam-mode-map (kbd "C-c k p") #'gkroam-toggle-prettify)
 (define-key gkroam-mode-map (kbd "C-c k t") #'gkroam-toggle-brackets)
 (define-key gkroam-mode-map (kbd "C-c k D") #'gkroam-toggle-dynamic)
 (define-key gkroam-mode-map (kbd "C-c k g") #'gkroam-update)
 )

(provide 'gkroam-config)
;;; gkroam-config.el ends here
