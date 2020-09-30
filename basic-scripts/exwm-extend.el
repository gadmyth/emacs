;;; package --- exwm-extend.el
;;; Commentary:
;;; Code:

(require 'exwm)

(defmacro exwm-start-process (name command)
  "Define a process with NAME and COMMAND."
  `(lambda ()
     (interactive)
     (start-process ,name nil ,command)))

(defmacro exwm-start-shell-command-process (name command)
  "Define a shell command process with NAME and COMMAND, the COMMAND run and blocked."
  `(lambda ()
     (interactive)
     (start-process-shell-command ,name nil ,command)))

(defmacro exwm-call-shell-command (command)
  "Define a shell command process with COMMAND, the COMMAND return immediately."
  `(lambda ()
     (interactive)
     (call-process-shell-command ,command)))

(defmacro exwm-exec-delay (delay &rest body)
  "Exec the BODY after DELAY seconds."
  `(run-with-timer
    ,delay nil
    (lambda ()
      ,@body)))

(defmacro exwm-exec-function-delay (delay function)
  "Exec the FUNCTION after DELAY seconds."
  `(run-with-timer
    ,delay nil ,function))

;; start some application after exwm init
(add-hook 'emacs-startup-hook (lambda ()
                                (interactive)
                                (exwm-exec-function-delay 1 (exwm-call-shell-command "wmname LG3D"))
                                (funcall (exwm-start-shell-command-process "xscreensaver" "xscreensaver"))
                                (funcall (exwm-start-shell-command-process "network" "nm-applet"))
                                (funcall (exwm-call-shell-command "blueberry-tray"))))

;; set char-mode as default
(setq exwm-manage-configurations '((t char-mode t)))

;; set force tiling
(setq exwm-manage-force-tiling t)

;; set char-mode as default
(setq exwm-workspace-number 10)

(require 'exwm-systemtray)
(setq exwm-systemtray-height 25)
(exwm-systemtray-enable)

(require 'exwm-config)
(exwm-config-default)

(require 'exwm-randr)
(exwm-randr-enable)

;; config global keys
(exwm-input-set-key [?\s-r] #'exwm-input-toggle-keyboard)
(exwm-input-set-key [?\s-f] (exwm-start-process "firefox" "firefox"))
(exwm-input-set-key [?\s-v] (exwm-start-process "vim" "gvim"))
(exwm-input-set-key [?\M-\s-s] (exwm-call-shell-command "systemctl suspend"))
(exwm-input-set-key (kbd "<M-s-delete>") (exwm-call-shell-command "xscreensaver-command -lock"))
(exwm-input-set-key (kbd "<s-return>") (exwm-start-process "terminal" "xfce4-terminal"))
(exwm-input-set-key [?\s-p] (exwm-start-process "appfinder" "xfce4-appfinder"))
(exwm-input-set-key [?\s-t] (exwm-start-shell-command-process "file manager" "Thunar"))

;; add workspace move global keys
(let ((workspace-numbers (number-sequence 0 9))
      (keys ")!@#$%^&*("))
  (seq-doseq (num workspace-numbers)
    (let* ((idx num)
           (key (aref keys idx)))
      (exwm-input-set-key (kbd (format "s-%c" key))
                          `(lambda ()
                             (interactive)
                             (exwm-workspace-move-window ,num))))))

(provide 'exwm-extend)
;;; exwm-extend.el ends here
