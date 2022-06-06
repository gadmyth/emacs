;;; package --- f-zone-key-bindings.el
;;; Commentary:
;;; Code:


(require 'eyebrowse+)
(require 'customized-dir)
(require 'minibuffer+)
(require 'list-scratch)

;; buffer
(global-set-key (kbd "<f1>") (toggle-minibuffer 'switch-scratch-buffers))
(global-set-key (kbd "<M-f1>") 'new-buffer)

;; eyebrowse buffer config
(global-set-key (kbd "<f2>") (toggle-minibuffer 'eyebrowse-list-bookmarks))
(global-set-key (kbd "<M-f2>") (toggle-minibuffer 'eyebrowse-modify-bookmark))
(global-set-key (kbd "<C-f2>") 'eyebrowse-switch-bookmarks)

;; eyebrowse buffer config
(global-set-key (kbd "<f3>") (toggle-minibuffer 'eyebrowse-switch-buffer))
(global-set-key (kbd "<M-f3>") (toggle-minibuffer 'eyebrowse-modify-buffer-config))
(global-set-key (kbd "<C-f3>") 'eyebrowse-switch-other-buffer)

;; customized dir
(global-set-key (kbd "<f4>") (toggle-minibuffer 'switch-to-customized-dir))
;; *** not implemented ***
(global-set-key (kbd "<C-f4>") 'cusomized-dir-action)

;; eyebrowse config
(global-set-key (kbd "<f5>") (toggle-minibuffer 'eyebrowse-list-configs))
(global-set-key (kbd "<M-f5>") (toggle-minibuffer 'eyebrowse-modify-config))
(global-set-key (kbd "<C-f5>") 'eyebrowse-last-window-config)

;; TODO: move to editors.el
;; (global-set-key (kbd "<f5>") '(lambda () (interactive)
;;                                 (let ((index (string-match "\\(.*\\)\\.\\(.\\)" (buffer-name)))
;;                                       (prename (match-string 1 (buffer-name)))
;;                                       (suffix (match-string 2 (buffer-name))))
;;                                   (when (equal index 0)
;;                                     (if (equal "m" suffix)
;;                                         (find-file (concatenate 'string prename ".h"))
;;                                       (if (equal "h" suffix)
;;                                           (find-file (concatenate 'string prename ".m"))))))))

;; library
(global-set-key (kbd "<f6>") (toggle-minibuffer 'find-library))
(global-set-key (kbd "<M-f6>") (toggle-minibuffer 'require-library))
(global-set-key (kbd "<C-f6>") (toggle-minibuffer 'load-library))

;; list-scratch
(global-set-key (kbd "<f12>") (toggle-minibuffer 'list-scratch))

(provide 'f-zone-key-bindings)
;;; f-zone-key-bindings.el ends here
