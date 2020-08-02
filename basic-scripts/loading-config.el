;;; package --- loading-config.el
;;; Commentary:
;;; Code:

(require 'async-config)

(setq gc-cons-threshold (* 50 1000 1000))
(setq garbage-collection-messages t)

(add-hook
 'emacs-startup-hook
 (lambda ()
   (call-after
    1
    ;; This could use (emacs-init-time)
    (message "**** Emacs startup end in %s with %d garbage collections *****"
             (emacs-init-time)
             gcs-done))))

(provide 'loading-config)
;;; loading-config.el ends here