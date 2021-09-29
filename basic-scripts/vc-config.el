;;; package --- vc-config.el
;;; Commentary:
;;; Code:

(add-hook
 'vc-dir-mode-hook
 (lambda ()
   (message "vc-dir-mode loaded, now add extra keymap to vc-dir-mode-map!")
   (let ((stash-map (make-sparse-keymap)))
     (define-key vc-dir-mode-map "s" stash-map)
     (define-key stash-map "s" 'vc-git-stash)
     (define-key stash-map "p" 'vc-git-stash-pop)
     (define-key stash-map "a" 'vc-git-stash-apply)
     (define-key stash-map "d" 'vc-git-stash-delete)
     (define-key stash-map "h" 'vc-git-stash-show))
   nil))

(provide 'vc-config)
;;; vc-config.el ends here
