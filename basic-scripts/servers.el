;;; package --- servers.el
;;; Commentary:
;;; Code:

(require 'server)

(unless (or (daemonp)
            (server-running-p))
  (ignore-errors
    (server-start)))

(provide 'servers)
;;; servers.el ends here
