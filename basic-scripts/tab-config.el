;;; package --- tab-config.el
;;; Commentary:
;;; Code:


(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq-default default-tab-width 4)
(setq-default tab-width 4)

(require 'nxml-mode)
(setq nxml-child-indent 4)

(provide 'tab-config)
;;; tab-config.el ends here
