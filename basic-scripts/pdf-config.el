;;; package --- pdf-config.el
;;; Commentary:
;;; Code:

(require-safely
 'pdf-tools
 (pdf-tools-install)
 (setq-default pdf-view-display-size 3.0)
 (add-hook 'pdf-view-mode-hook
           (lambda ()
             (pdf-view-midnight-minor-mode)
             (let ((params (frame-parameters)))
               (setq pdf-view-midnight-colors
                     (cons (alist-get 'foreground-color params)
                           (alist-get 'background-color params)))))))

(provide 'pdf-config)
;;; pdf-config.el ends here
