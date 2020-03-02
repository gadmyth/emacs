;;; package --- web-config.el
;;; Commentary:
;;; Code:

(require-packages-if-installed
 '('web-mode 'emmet-mode 'js2-mode 'js2-refactor)

 (setq auto-mode-alist
       (append
        '(("\\.js\\'" . js2-mode))
        '(("\\.html\\'" . web-mode))
        auto-mode-alist))

 (add-hook 'web-mode-hook
           '(lambda () my-web-mode-indent-setup ()
              (setq web-mode-markup-indent-offset 4) ; web-mode, html tag
              (setq web-mode-css-indent-offset 4)    ; web-mode, css
              (setq web-mode-code-indent-offset 4)   ; web-mode, js code
              ))

 (add-hook 'js2-mode-hook #'js2-refactor-mode)
 (js2r-add-keybindings-with-prefix "C-c C-m")

 (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
 (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
 )

(provide 'web-config)
;;; web-config.el ends here