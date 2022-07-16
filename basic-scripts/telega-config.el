;;; package --- telega-config.el
;;; Commentary:
;;; Code:

(require-package
 'telega
 (when (and (bound-and-true-p *socks5-proxy-server*)
            (bound-and-true-p *socks5-proxy-port*))
   (setq telega-proxies
         (list
          `(:server ,*socks5-proxy-server*
                    :port ,*socks5-proxy-port*
                    :enable t
                    :type (:@type "proxyTypeSocks5"))
          )))
 )

(provide 'telega-config)
;;; telega-config.el ends here
