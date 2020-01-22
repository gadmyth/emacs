;;; package --- basic-config.el
;;; Commentary:
;;; Code:

(defalias 'yes-or-no-p 'y-or-n-p)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(global-unset-key (kbd "C-SPC"))

(require 'warnings)
(setq display-warning-minimum-level :emergency)

(provide 'basic-config)
;;; basic-config.el ends here