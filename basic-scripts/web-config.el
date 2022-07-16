;;; package --- web-config.el
;;; Commentary:
;;; Code:

(require-packages
 '(web-mode)
 (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
 (add-hook 'web-mode-hook
           (lambda ()
             ; web-mode, html tag
             (setq web-mode-markup-indent-offset 2)
             ; web-mode, css
             (setq web-mode-css-indent-offset 2)
             ; web-mode, js code
             (setq web-mode-code-indent-offset 2)
             ))
 )

(require-packages
 '(js2-mode js2-refactor)
 (setq auto-mode-alist
       (append
        '(("\\.js\\'" . js2-mode))
        '(("\\.html\\'" . web-mode))
        auto-mode-alist))
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-m")
 )

(require-packages
 '(emmet-mode)
 ;; Auto-start on any markup modes
 (add-hook 'sgml-mode-hook 'emmet-mode)
 ;; enable Emmet's css abbreviation.
 (add-hook 'css-mode-hook  'emmet-mode)
 )

(provide 'web-config)
;;; web-config.el ends here