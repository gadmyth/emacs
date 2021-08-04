;;; package --- notifications.el
;;; Commentary:
;;; Code:

(require 'async)
(require 'dates)

(defun start-notify (message arg2)
  "Show MESSAGE as notification after some minutes or at some certain time of ARG2."
  (interactive
   (list
    (read-string "notification to send: " nil nil nil)
    (if (null current-prefix-arg)
        (read-number "after minitues: " 0)
      (read-string "at the time: " (show-current-time-string)))))
  (let ((minutes (if (null current-prefix-arg)
                     arg2 (/ (- (string-to-number (string-to-timestamp arg2))
                                (string-to-number (current-timestamp)))
                             60.0))))
    (when (> minutes 0)
      (do-start-notify message minutes))))

(defun do-start-notify (message minutes)
  "Show MESSAGE as notification after some MINUTES."
  (async-start
   `(lambda ()
      (let ((seconds (* 60 ,minutes)))
        (sleep-for seconds) seconds))
   `(lambda (arg)
     (with-output-to-temp-buffer "*notification*"
       (print ,message)
       (print (format-time-string "%Y-%m-%d %a %H:%M" (current-time)))))))


(global-set-key (kbd "C-x s") 'start-notify)

(provide 'notifications)
;;; notifications.el ends here
