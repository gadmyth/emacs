;;; package --- loading-config.el
;;; Commentary:
;;; Code:

(require 'async-config)

(setq gc-cons-threshold (* 50 1000 1000))

(add-hook
 'emacs-startup-hook
 (lambda ()
   (call-after
    1
    (message "**** Emacs startup end in %s with %d garbage collections *****"
             (float-time (time-subtract after-init-time before-init-time))
             gcs-done))))

(provide 'loading-config)
;;; loading-config.el ends here