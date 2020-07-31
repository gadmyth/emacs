;;; package --- f-zone-key-bindings.el
;;; Commentary:
;;; Code:


(require 'eyebrowse+)
(require 'customized-dir)
(require 'ivy)

(defmacro toggle-minibuffer (func)
  "When in a minibuffer, exit the minibuffer, or call the FUNC."
  "ATTENION: this can't pass the args."
  `(lambda (&rest args)
    (interactive)
    (if (window-minibuffer-p)
        (minibuffer-keyboard-quit)
      (funcall-interactively ,func args))))

;; buffer
(global-set-key (kbd "<f1>") (toggle-minibuffer 'switch-buffer-default-scratch))
(global-set-key (kbd "<M-f1>") 'new-buffer)

;; eyebrowse config
(global-set-key (kbd "<f2>") (toggle-minibuffer 'eyebrowse-list-configs))
(global-set-key (kbd "<M-f2>") 'eyebrowse-modify-config)
(global-set-key (kbd "<C-f2>") 'eyebrowse-last-window-config)

;; eyebrowse buffer config
(global-set-key (kbd "<f3>") (toggle-minibuffer 'eyebrowse-switch-buffer))
(global-set-key (kbd "<M-f3>") 'eyebrowse-modify-buffer-config)
(global-set-key (kbd "<C-f3>") 'eyebrowse-switch-other-buffer)

;; customized dir
(global-set-key (kbd "<f4>") (toggle-minibuffer 'switch-to-customized-dir))
;; *** not implemented ***
(global-set-key (kbd "<C-f4>") 'cusomized-dir-action)


(global-set-key (kbd "<f5>") '(lambda () (interactive)
                                (let ((index (string-match "\\(.*\\)\\.\\(.\\)" (buffer-name)))
                                      (prename (match-string 1 (buffer-name)))
                                      (suffix (match-string 2 (buffer-name))))
                                  (when (equal index 0)
                                    (if (equal "m" suffix)
                                        (find-file (concatenate 'string prename ".h"))
                                      (if (equal "h" suffix)
                                          (find-file (concatenate 'string prename ".m"))))))))

;; library
(global-set-key (kbd "<f6>") 'find-library)
(global-set-key (kbd "<M-f6>") 'require-library)

(provide 'f-zone-key-bindings)
;;; f-zone-key-bindings.el ends here
