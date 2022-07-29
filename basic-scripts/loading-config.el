;;; package --- loading-config.el
;;; Commentary:
;;; Code:


(setq gc-cons-threshold (* 1000 1000 1000))
(setq garbage-collection-messages t)

(add-hook
 'emacs-startup-hook
 (lambda ()
   (with-timeout (1 nil)
     ;; This could use (emacs-init-time)
     (message "**** Emacs startup end in %s with %d garbage collections *****"
              (emacs-init-time)
              gcs-done))))


(provide 'loading-config)
;;; loading-config.el ends here
