;;; package --- emoji-config.el
;;; Commentary:
;;; Code:

(add-hook
 'swift-mode-hook
 (lambda ()
   (set-fontset-font
    t 'symbol
    (font-spec :family (if (eq window-system 'ns) "Apple Color Emoji" "Symbola") nil 'prepend))))

(require-if-installed 'ac-emoji
                      (add-hook
                       'swift-mode-hook
                       (lambda ()
                         (mapc 
                          (lambda (item)
                            (let* ((key (plist-get item :key))
                                   (key (substring key 1 (1- (length key))))
                                   (codepoint (plist-get item :codepoint)))
                              (define-abbrev swift-mode-abbrev-table key codepoint)))
                          ac-emoji--data)))

                      (add-hook 'swift-mode-hook 'ac-emoji-setup))

(provide 'emoji-config)

;;; emoji-config.el ends here