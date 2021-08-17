;;; package --- telega-config.el
;;; Commentary:
;;; Code:

(defvar *socks5-proxy-server* nil)
(defvar *socks5-proxy-port* nil)

(require-if-installed
 'telega
 (when (and *socks5-proxy-server* *socks5-proxy-port*)
   (setq telega-proxies
         (list
          '(:server *socks5-proxy-server*
                    :port *socks5-proxy-port*
                    :enable t
                    :type (:@type "proxyTypeSocks5"))
          )))
 )

(provide 'telega-config)
;;; telega-config.el ends here
