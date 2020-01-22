;;; package --- work.el
;;; Commentary:
;;; Code:

(require 'org)

(defvar WORK-FILE)
(setq WORK-FILE (expand-file-name "~/work.org"))

(defvar WORK-FILE-HTML)
(setq WORK-FILE-HTML (expand-file-name "~/work.html"))

(defun visit-work-file ()
  "."
  (interactive)
  (if (file-exists-p WORK-FILE)
      (find-file-other-window WORK-FILE)
    (progn
      (new-buffer "work.org")
      (switch-to-buffer-other-window "work.org")
      (write-file WORK-FILE))))

(defun visit-work-html()
  "."
  (interactive)
  (if (file-exists-p WORK-FILE-HTML)
      (shell-command-to-string (format "open %s" WORK-FILE-HTML))))

(defun update-work-html()
  "."
  (interactive)
  (org-html-export-to-html))

(require 'dired)
(add-hook 'dired-mode-hook (lambda () (define-key dired-mode-map (kbd "<f7>") 'org-capture-dired-file)))

(defun open-current-buffer()
  "."
  (interactive)
  (let* ((buffer (current-buffer))
         (file-name (buffer-file-name buffer)))
    (if (and (not (null file-name)) (file-exists-p file-name))
        (progn
          (shell-command-to-string (format "open %s" file-name))
          (message "Open succeed: %s" file-name))
      (message "File doesn't not exists!"))))

(defun show-buffer-in-another-window (buffer)
  "BUFFER: ."
  (interactive "bSelect a buffer: ")
  (let ((origin-buffer (current-buffer)))
    (switch-to-buffer-other-window buffer)
    (switch-to-buffer-other-window origin-buffer)))

(defun show-buffer-in-another-frame (buffer)
  "BUFFER: ."
  (interactive "bSelect a buffer: ")
  (let ((origin-buffer (current-buffer)))
    (switch-to-buffer-other-frame buffer)
    (switch-to-buffer-other-frame origin-buffer)))

(defun xah-insert-random-string (num)
  "NUM, Insert a random alphanumerics string of length 5.
The possible chars are 0 to 9, and a to z (lower case).
Call `universal-argument' before for different count.
URL `http://ergoemacs.org/emacs/elisp_insert_random_number_string.html'
Version 2017-05-24"
  (interactive "P")
  (let* ((charset "1234567890abcdefghijklmnopqrstuvwxyz")
         (base-count (length charset))
         (random-string ""))
    (dotimes (_ (if (numberp num) (abs num) 5))
      (let* ((char (elt charset (random base-count)))
             (str (concat random-string (char-to-string char))))
        (setq random-string str)))
    random-string))

(defun create-test-tmp-file (extension)
  "EXTENSION."
  (interactive "sfile extension: ")
  (new-buffer (format "test_%s.%s" (xah-insert-random-string 5) extension))
  (write-file (format "/tmp/%s" (buffer-name))))

(defun copy-buffername-without-extension ()
  "."
  (interactive)
  (let* ((buffername (buffer-name))
         (base-name (file-name-base buffername)))
    (kill-new base-name)
    (message base-name)
    base-name))

(defun insert-uuid ()
  (interactive)
  (let ((uuid (replace-regexp-in-string "-" "" (org-id-uuid))))
    (insert uuid)))

(provide 'work)
;;; work.el ends here