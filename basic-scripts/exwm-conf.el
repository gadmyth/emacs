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

(defun exwm-input-do-release-keyboard ()
  "Toggle between 'line-mode' and 'char-mode'."
  (interactive)
  (exwm--log)
  (when (derived-mode-p 'exwm-mode)
    (call-interactively #'exwm-input-release-keyboard)
    (exwm-input--update-focus (selected-window))))

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
                   `([?\M-\s-r] . exwm-input-do-release-keyboard)
                   `([?\s-f] . ,(exwm-start-process "firefox" "firefox"))
                   `([?\s-v] . ,(exwm-start-process "vim" "gvim"))
                   `([?\M-\s-s] . ,(exwm-exec-shell-command "suspend" "systemctl suspend"))
                   `(,(kbd "<M-s-delete>") . ,(exwm-exec-shell-command "lock screen" "xscreensaver-command -lock"))
                   `(,(kbd "<s-return>") . ,(exwm-start-process "terminal" "xfce4-terminal"))
                   `([?\s-p] . ,(exwm-start-process "appfinder" "xfce4-appfinder"))
                   `([?\s-t] . ,(exwm-exec-shell-command "file manager" "Thunar")))

;; add workspace move keys
(let ((workspace-numbers (number-sequence 0 9))
      (keys ")!@#$%^&*("))
  (seq-doseq (num workspace-numbers)
    (let* ((idx num)
           (key (aref keys idx))
           (exwm-key `(,(kbd (format "s-%c" key)) .
                       (lambda () (interactive) (exwm-workspace-move-window ,num)))))
      (add-to-list 'exwm-input-global-keys exwm-key))))

(provide 'exwm-conf)
;;; exwm-conf.el ends here
