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
         (file-name (buffer-file-name buffer))
         (command (pcase system-type ('darwin "open") ('gnu/linux "exo-open"))))
    (if (and (not (null file-name)) (file-exists-p file-name) command)
        (progn
          (shell-command (format "%s %s" command file-name))
          (message "Open succeed: %s %s" command file-name))
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


(require 'uuid)
(defun insert-short-uuid ()
  "Insert lowercase uuid without dash."
  (interactive)
  (let ((uuid (replace-regexp-in-string "-" "" (uuid-string))))
    (insert uuid)))

(defun insert-normal-uuid ()
  "Insert uppercase uuid with dash."
  (interactive)
  (let ((uuid (upcase (uuid-string))))
    (insert uuid)))

(defun prepend-major-mode-language ()
  "PREPEND-MAJOR-MODE-LANGUAGE ."
  (interactive)
  (goto-char 0)
  (let* ((major-mode-string (symbol-name major-mode))
         (major-mode-language (replace-regexp-in-string "-mode" "" major-mode-string)))
    (insert (format "# -*- %s -*-\n" major-mode-language))))

(defvar +scriptlets-directory+ (expand-file-name "scriptlets" +emacs-context-directory+))

(defvar *intellij-command-list*
  `((intellij-community . "open-file-in-intellij-idea")
    (intellij-ultimate . "open-file-in-intellij-idea-ultimate")))

(defun open-current-file-in-intellij-idea ()
  (interactive)
  (when-let ((path (buffer-file-name (current-buffer))))
    (let ((app (completing-read "Select the intellij idea app: "
                                *intellij-command-list* nil t)))
      (shell-command-to-string (format "source %s; %s %d %s"
                                       (expand-file-name "idea.sh" +scriptlets-directory+)
                                       (alist-get app *intellij-command-list* nil nil #'string=)
                                       (line-number-at-pos)
                                       path)))))

(global-set-key (kbd "<f11>") #'open-current-file-in-intellij-idea)

(provide 'work)
;;; work.el ends here