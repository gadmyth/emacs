;;; package --- .emacs.quick.el
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "~/emacs/basic-scripts"))
(add-to-list 'load-path (expand-file-name "~/emacs/el-extends"))

(require 'basic-config)
(require 'kill-ring)
(require 'tab-config)

(provide '.emacs.quick)
;;; .emacs.quick.el ends here
