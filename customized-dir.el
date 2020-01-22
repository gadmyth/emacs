;;; package --- customized-dir.el
;;; Commentary:
;;; Code:

(require 'ivy)

(defun new-buffer (name)
  "NAME: ."
  (interactive (list (read-string "Create buffer(*scratch*): " nil nil "*scratch*")))
  (let ((buffer (generate-new-buffer name)))
    (switch-to-buffer buffer)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))

(defun switch-buffer (name)
  "NAME: ."
  (interactive (list (read-string "Switch buffer(*scratch*): " nil nil "*scratch*")))
  (let ((buffer (get-buffer name)))
    (if (buffer-live-p buffer)
        (progn
          (switch-to-buffer buffer)
          ;(funcall (and initial-major-mode))
          (setq buffer-offer-save t))
      (message (format "The buffer %s doesn't exist!" name)))))

(defun switch-to-customized-dir ()
  "."
  (interactive)
  (ivy-read "Switch to dir: " *customized-dir*
            :action (lambda (dir)
                      (dired dir))))

(defvar +customized-dir-file-name+ "~/.customized-dir-save")
(defvar +dired-al-mode-header+ "  drwx------.  0 user user     4096 Mar  0 00:00 ")
(defvar *customized-dir* nil)
(defun load-customized-dir ()
  "."
  (interactive)
  (when (file-readable-p +customized-dir-file-name+)
    (with-temp-buffer
      (insert-file-contents +customized-dir-file-name+)
      (goto-char (point-min))
      (mapc #'(lambda (dir)
                (unless (member dir *customized-dir*)
                  (push dir *customized-dir*)))
            (read (current-buffer))))))

(defun save-customized-dir-without-confirm ()
  "."
  (with-temp-file +customized-dir-file-name+
    (print *customized-dir* (current-buffer))))

(defun save-customized-dir (confirmed)
  "CONFIRMED: ."
  (interactive (list (y-or-n-p (format "Sure to add %s to customized-dir? " default-directory))))
  (if confirmed
      (progn
        (save-customized-dir-without-confirm)
        (message "Save succeed!"))
    (message "Save canceled!")))

(defun customized-dired ()
  "."
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create "*Customized*"))
  (widen)
  (kill-all-local-variables)
  (setq buffer-read-only nil)
  (erase-buffer)
  (mapc (lambda (dir)
          (insert +dired-al-mode-header+ dir "\n"))
        *customized-dir*)
  (goto-char (point-min))
  (setq default-directory "/")
  (dired-mode default-directory "-al")
  (make-local-variable 'dired-sort-inhibit)
  (setq dired-sort-inhibit t)
  (set (make-local-variable 'dired-subdir-alist)
       (list (cons default-directory (point-min-marker))))
  (set (make-local-variable 'dired-subdir-switches) nil)
  (setq buffer-read-only nil)
  (insert "  " default-directory ":\n")
  (let ((point (point)))
    (insert "  " "wildcard dirs" "\n")
    (dired-insert-set-properties point (point)))
  (setq buffer-read-only t))

(defun add-customized-dir (confirm)
  "CONFIRM: ."
  (interactive (list (y-or-n-p (format "Sure to add %s to customized-dir? " default-directory))))
  (if confirm
      (progn
        (let ((dir (expand-file-name default-directory)))
          (unless (member dir *customized-dir*)
            (push dir *customized-dir*))
          (message "%s added." dir)))
    (message "Action canceled!")))

(defun remove-customized-dir (confirm)
  "."
  (interactive (list (y-or-n-p (format "Sure to remove %s to customized-dir? " default-directory))))
  (if confirm
      (let ((dir (expand-file-name default-directory)))
        (if (member dir *customized-dir*)
          (progn
            (setq *customized-dir* (delete dir *customized-dir*))
            (message "%s removed." dir))
          (message "%s is not a customized dir!" dir)))
    (message "Action canceled!")))

(defun customized-dir-init ()
  "."
  (add-hook 'kill-emacs-hook 'save-customized-dir-without-confirm)
  (load-customized-dir))


(require 'dired)
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map "V"
              '(lambda () (interactive)
                 (let ((file (dired-get-file-for-visit)))
                   (if (file-directory-p file)
                       (magit-status file)))))

            (define-key dired-mode-map "v"
              '(lambda () (interactive)
                 (let ((file (dired-get-file-for-visit)))
                   (if (file-directory-p file)
                       (vc-dir file)))))))

;; ace-jump-buffer
(require 'avy-config)
;; the <f4> is empty

(provide 'customized-dir)
;;; customized-dir.el ends here
