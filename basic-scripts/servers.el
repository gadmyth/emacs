;;; package --- servers.el
;;; Commentary:
;;; Code:

(require 'server)

(unless (server-running-p)
  (ignore-errors
    (server-start)))

(provide 'servers)
;;; servers.el ends here
