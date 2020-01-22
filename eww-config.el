;;; package --- eww-config.el
;;; Commentary:
;;; Code:


(defun toggle-browser-function ()
  "."
  (interactive)
  (if (equal browse-url-browser-function 'eww-browse-url)
      (setq browse-url-browser-function 'browse-url-default-browser)
    (setq browse-url-browser-function 'eww-browse-url))
  (message "Current browser function is: %S" browse-url-browser-function))

(provide 'eww-config)
;;; eww-config.el ends here