;;; package --- exwm-conf.el
;;; Commentary:
;;; Code:


(let ((desktop-session (shell-command-to-string "echo -n $DESKTOP_SESSION")))
  (if (string-equal "exwm" desktop-session)
      (require-safely 'exwm (require 'exwm-extend))
    (message "The DESKTOP_SESSION is not exwm, stop load the exwm")))

(provide 'exwm-conf)
;;; exwm-conf.el ends here
