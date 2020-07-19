;;; package --- shells.el
;;; Commentary:
;;; Code:

(defconst +LOAD-SHELL-PROFILE+ (if (eq window-system 'ns)
                                   "source ~/.profile; source ~/.bash_alias; source ~/mybash_profile"
                                 "source ~/.bashrc"))

(let* ((command (format "%s; echo $PATH" +LOAD-SHELL-PROFILE+))
       (path (shell-command-to-string command)))
  (setenv "PATH" path)
  (setq exec-path (append (split-string-and-unquote path ":") exec-path)))

;; The following will cause the bash problem: "bash: shell_session_update: command not found"
;; (setq shell-command-switch "-ic")


(require 'eshell)
(require 'em-alias)

(defun eshell-load-bash-aliases ()
  "Read bash aliases from Bash and insert them into the list of eshell aliases."
  (interactive)
  (setq eshell-command-aliases-list nil)
  (let* ((command (format "%s; alias" +LOAD-SHELL-PROFILE+))
         (shell-alias (shell-command-to-string command))
         alias-array alias-list)
    (setq shell-alias (replace-regexp-in-string "alias " "" shell-alias))
    (setq shell-alias (replace-regexp-in-string "='" " " shell-alias))
    (setq shell-alias (replace-regexp-in-string "'\n" "\n" shell-alias))
    (setq shell-alias (replace-regexp-in-string "=\"" " " shell-alias))
    (setq shell-alias (replace-regexp-in-string "\"\n" "\n" shell-alias))
    (setq alias-array (split-string shell-alias "\n"))
    (dolist (alias-line alias-array)
      (when (string-match "\\(.*?\\) \\(.*\\)" alias-line)
        (let ((alias-name
               (substring alias-line (match-beginning 1) (match-end 1)))
              (alias-command
               (substring alias-line (match-beginning 2) (match-end 2))))
          (push (list alias-name alias-command) alias-list))))
    (setq eshell-command-aliases-list alias-list)))

(add-hook 'eshell-mode-hook 'eshell-load-bash-aliases)

(provide 'shells)
;;; shells.el ends here
