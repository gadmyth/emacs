;;; package --- exwm-conf.el
;;; Commentary:
;;; Code:

(require 'exwm)

(defmacro exwm-start-process (name command)
  "Define a process with NAME and COMMAND."
  `(lambda ()
     (interactive)
     (start-process ,name nil ,command)))

(defmacro exwm-exec-shell-command (name command)
  "Define a shell command process with NAME and COMMAND."
  `(lambda ()
     (interactive)
     (start-process-shell-command ,name nil ,command)))

(defun add-multi-to-list (list &rest elements)
  "Add all the ELEMENTS to LIST."
  (dolist (element elements)
    (add-to-list list element)))

;; start xscreensaver after exwm init
(add-hook 'exwm-init-hook (exwm-start-process "xscreensaver" "xscreensaver"))

(require 'exwm-systemtray)
(exwm-systemtray-enable)

(require 'exwm-config)
(exwm-config-default)

(require 'exwm-randr)
(exwm-randr-enable)

;; config global keys
(add-multi-to-list 'exwm-input-global-keys
                   `([?\s-f] . ,(exwm-start-process "firefox" "firefox"))
                   `([?\s-v] . ,(exwm-start-process "vim" "gvim"))
                   `([?\M-\s-s] . ,(exwm-exec-shell-command "suspend" "systemctl suspend"))
                   `(,(kbd "<M-s-delete>") . ,(exwm-exec-shell-command "lock screen" "xscreensaver-command -lock"))
                   `([?\s-p] . ,(exwm-start-process "appfinder" "xfce4-appfinder")))

(provide 'exwm-conf)
;;; exwm-conf.el ends here
