;;; package --- key-bindings.el
;;; Commentary:
;;; Code:

(require 'eyebrowse-config)
(require 'customized-dir)

;;; F zone
(global-set-key (kbd "<f1>") 'switch-buffer)
(global-set-key (kbd "<M-f1>") 'switch-to-customized-dir)

(global-set-key (kbd "<f2>") 'eyebrowse-list-configs)
(global-set-key (kbd "<M-f2>") '(lambda () (interactive) (switch-to-buffer (other-buffer))))

(global-set-key (kbd "<f3>") 'eyebrowse-list-actions)
(global-set-key (kbd "<M-f3>") 'eyebrowse-last-window-config)

(require 'ivy)
(global-set-key (kbd "<f4>") 'ivy-switch-buffer)
(global-set-key (kbd "<M-f4>") 'new-buffer)

(global-set-key (kbd "<f5>") '(lambda () (interactive)
                                (let ((index (string-match "\\(.*\\)\\.\\(.\\)" (buffer-name)))
                                      (prename (match-string 1 (buffer-name)))
                                      (suffix (match-string 2 (buffer-name))))
                                  (when (equal index 0)
                                    (if (equal "m" suffix)
                                        (find-file (concatenate 'string prename ".h"))
                                      (if (equal "h" suffix)
                                          (find-file (concatenate 'string prename ".m"))))))))

(global-set-key (kbd "<f6>") 'find-library)
(global-set-key (kbd "<M-f6>") 'require-library)


(provide 'key-bindings)
;;; key-bindings.el ends here
