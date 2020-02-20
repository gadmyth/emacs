;;; package --- notifications.el
;;; Commentary:
;;; Code:

(require 'async)

(defun start-notify (message time)
  "Show MESSAGE as notification after TIME seconds."
  (interactive "snotification to send: \nnafter seconds: ")
  (async-start
   `(lambda () (sleep-for ,time) ,time)
   `(lambda (arg)
     (with-output-to-temp-buffer "notification"
       (print ,message)
       (print (format-time-string "%Y-%m-%d %a %H:%M" (current-time)))))))


(global-set-key (kbd "C-x s") 'start-notify)

(provide 'notifications)
;;; notifications.el ends here
