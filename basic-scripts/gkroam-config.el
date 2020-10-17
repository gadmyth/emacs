;;; package --- gkroam-config.el
;;; Commentary:
;;; Code:

(require 'gkroam)

(let ((dir *gkroam-default-root-dir*))
  (when (and (file-exists-p dir)
             (f-directory-p dir))
    (setq gkroam-root-dir dir)))

(setq gkroam-prettify-p t
      gkroam-show-brackets-p t
      gkroam-use-default-filename t
      gkroam-window-margin 4)

(global-set-key (kbd "C-c g I") #'gkroam-index)
(global-set-key (kbd "C-c g d") #'gkroam-daily)
(global-set-key (kbd "C-c g f") #'gkroam-find)
(global-set-key (kbd "C-c g i") #'gkroam-insert)
(global-set-key (kbd "C-c g c") #'gkroam-capture)
(global-set-key (kbd "C-c g e") #'gkroam-link-edit)
(global-set-key (kbd "C-c g n") #'gkroam-smart-new)
(global-set-key (kbd "C-c g p") #'gkroam-toggle-prettify)
(global-set-key (kbd "C-c g t") #'gkroam-toggle-brackets)
(global-set-key (kbd "C-c g D") #'gkroam-toggle-dynamic)
(global-set-key (kbd "C-c g g") #'gkroam-update)

(provide 'gkroam-config)
;;; gkroam-config.el ends here
