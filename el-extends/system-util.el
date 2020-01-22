;;; package --- system-util.el
;;; Commentary:
;;; Code:


(defun open-with-vim ()
  "."
  (interactive)
  (let* ((path (buffer-file-name))
         (command
          (if (eq window-system 'ns)
              (format "open -a MacVim %s" path)
            (format "gvim %s" path))))
    (shell-command-to-string command)))

(defun open-file-manager ()
  "."
  (interactive)
  (let* ((command
          (cond ((eq window-system 'ns) "open -a Finder")
                ((eq window-system 'x) "exo-open --launch FileManager"))))
    (shell-command-to-string command)))

(defun open-terminal ()
  "."
  (interactive)
  (let* ((command
          (cond ((eq window-system 'ns) "open -a Terminal")
                ((eq window-system 'x) "exo-open --launch TerminalEmulator"))))
    (shell-command-to-string command)))

(defun open-url (url)
  "URL: ."
  (interactive "surl: ")
  (let* ((command
          (cond ((eq window-system 'ns) (format "open -a /Applications/Google\\ Chrome.app %s" url))
                ((eq window-system 'x) (format "exo-open --launch WebBrowser %s" url)))))
    (shell-command-to-string command)))

(provide 'system-util)
;;; system-util.el ends here
