;;; package --- annotate-config.el
;;; Commentary:
;;; Code:

(require 'annotate)

(let ((map annotate-mode-map))
    (define-key map (kbd "C-x C-a a") #'annotate-annotate)
    (define-key map (kbd "C-x C-a d") #'annotate-delete-annotation)
    (define-key map (kbd "C-x C-a s") #'annotate-show-annotation-summary)
    (define-key map (kbd "C-x C-a ]")   #'annotate-goto-next-annotation)
    (define-key map (kbd "C-x C-a [")   #'annotate-goto-previous-annotation))

;; TODO: list all the annotations, list the annotation of current buffer


(provide 'annotate-config)
;;; annotate-config.el ends here
