;;; package --- annotate-config.el
;;; Commentary:
;;; Code:

(require 'annotate)

;; set up annotion file
(setq annotate-file *annotate-file*)

;; remove annotation
(defun annotate-remove-annotation ()
  "."
  (interactive)
  (when-let ((annotation (annotate-annotation-at (point))))
    (annotate--delete-annotation-chain annotation)))

(define-key annotate-mode-map (kbd "C-c d") 'annotate-remove-annotation)

;; TODO: list all the annotations, list the annotation of current buffer


(provide 'annotate-config)
;;; annotate-config.el ends here
