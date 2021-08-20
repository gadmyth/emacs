;;; package --- mail-config.el
;;; Commentary:
;;; Code:


(eval-when-compile (require 'cl))

(require-safely
 'mew
 (autoload 'mew "mew" nil t)
 (autoload 'mew-ned "mew" nil t))

(provide 'mail-config)
;;; mail-config.el ends here
