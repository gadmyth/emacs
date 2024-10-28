;;; package --- loading-config.el
;;; Commentary:
;;; Code:


;; 50M
(defconst +modern-gc-cons-threshold+ (* 100 1024 1024))
;; set threshold to maximum
(setq gc-cons-threshold most-positive-fixnum)

(setq garbage-collection-messages t)

(add-hook
 'emacs-startup-hook
 (lambda ()
   (with-timeout (1 nil)
     ;; This could use (emacs-init-time)
     (message "**** Emacs startup end in %s with %d garbage collections *****"
              (emacs-init-time)
              gcs-done)
     (setq gc-cons-threshold +modern-gc-cons-threshold+))))


(provide 'loading-config)
;;; loading-config.el ends here
