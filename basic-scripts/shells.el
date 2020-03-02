;;; package --- shells.el
;;; Commentary:
;;; Code:


(let ((path (if (eq window-system 'ns)
                (shell-command-to-string "source ~/.profile; source ~/mybash_profile; echo $PATH")
              (shell-command-to-string "source ~/.bashrc; echo $PATH"))))
  (setenv "PATH" path)
  (setq exec-path (append (split-string-and-unquote path ":") exec-path)))

;; The following will cause the bash problem: "bash: shell_session_update: command not found"
;; (setq shell-command-switch "-ic")

(provide 'shells)
;;; shells.el ends here

