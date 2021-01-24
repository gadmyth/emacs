;;; package --- servers.el
;;; Commentary:
;;; Code:

(require 'server)

(if (and (not (eq window-system 'x))
         (or (not (boundp 'server-process))
             (null server-process)))
    (ignore-errors
      (server-start)))

(provide 'servers)
;;; servers.el ends here
