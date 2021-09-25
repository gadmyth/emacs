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
      (read-string "at the time: " (current-time-normal-string)))))
  (let ((minutes (if (null current-prefix-arg)
                     arg2 (/ (- (string-to-timestamp arg2)
                                (current-timestamp))
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
      (with-current-buffer (get-buffer-create "*notification*")
        (read-only-mode 0)
        (goto-char (point-max))
        (insert (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)) "\n" ,message "\n\n")
        (read-only-mode t))
      (display-buffer "*notification*"))))


(global-set-key (kbd "C-x s") 'start-notify)

(provide 'notifications)
;;; notifications.el ends here
