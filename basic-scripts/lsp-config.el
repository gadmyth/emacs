;;; package --- lsp-config.el
;;; Commentary:
;;; Code:


(defvar +lsp-java-server-default-dir+ nil)

(when +lsp-java-server-default-dir+
  (require 'lsp)
  (require 'lsp-ui)
  (setq lsp-java-server-install-dir +lsp-java-server-default-dir+)

  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-enable nil)

  (require 'lsp-java-boot)
  (add-hook 'lsp-mode-hook #'lsp-lens-mode)
  (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)

  (require 'lsp-java)
  (add-hook 'java-mode-hook #'lsp)

  (require 'dap-java)
  (require 'dap-mode)
  (dap-auto-configure-mode)
  (global-set-key (kbd "M-'") 'lsp-find-implementation)
  )

(provide 'lsp-config)
;;; lsp-config.el ends here
