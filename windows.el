;;; package --- windows.el
;;; Commentary:
;;; Code:

(require 'windmove)

(windmove-default-keybindings)
(window-numbering-mode 1)

(setq winner-mode t)

(defun quit-help-window ()
  "."
  (interactive)
  (let ((window (get-buffer-window "*Help*")))
    (if (window-live-p window)
        (quit-window nil window))))
  
(defun quit-help-windows ()
  "."
  (dolist (window (window-list))
    (let* ((buffer (window-buffer window))
           (name (buffer-name buffer)))
      (if (and (string-prefix-p "*" name)
               (let ((first-char (substring name 1 2)))
                 (equal (upcase first-char) first-char)))
          (if (window-live-p window)
              (quit-window nil window))))))

(defun quit-temp-windows ()
  "."
  (interactive)
  (if (active-minibuffer-window)
      (minibuffer-keyboard-quit)
    (quit-help-windows)))

(global-set-key (kbd "C-x q") 'quit-temp-windows)

(provide 'windows)
;;; windows.el ends here
