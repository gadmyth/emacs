;;; package --- windows.el
;;; Commentary:
;;; Code:

(global-linum-mode 0)
(global-visual-line-mode t)
(global-auto-revert-mode t)

(require 'linum-relative)
(show-paren-mode 1)
(setq winner-mode t)
(setq column-number-mode t)
(setq line-number-mode t)
(setq size-indication-mode t)

(require 'windmove)
(windmove-default-keybindings)
(window-numbering-mode 1)


(defun quit-help-window ()
  "."
  (interactive)
  (if-let ((help-window (get-buffer-window "*Help*")))
      (quit-window nil help-window)))

(defun quit-help-windows ()
  "."
  (interactive)
  (dolist (window (window-list))
    (let* ((buffer (window-buffer window))
           (name (buffer-name buffer)))
      (if (and (string-prefix-p "*" name)
               (let ((first-char (substring name 1 2)))
                 (equal (upcase first-char) first-char)))
          (quit-window nil window)))))

(global-set-key (kbd "C-x q") 'quit-help-windows)

(provide 'windows)
;;; windows.el ends here
