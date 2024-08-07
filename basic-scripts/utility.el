;;; package --- utility.el
;;; Commentary:
;;; Code:

(require 'package)
(require 'wcy-desktop)

(eval-when-compile (require 'cl))

(defvar *find-grep-dired--dir* "~")

(defun find2-grep-dired (dir regexp)
  "DIR is the root directory of find command, REGEXP is the file's regular expression."
  (interactive (list (read-directory-name "What directory? "
										  *find-grep-dired--dir*)
					 (read-string "What to search? " (car kill-ring))))
  (setq *find-grep-dired--dir* dir)
  (isearch-update-ring regexp t)
  (find-dired dir
			  (concat "-not \\( -name .svn -prune \\) " "-type f -exec " grep-program " " find-grep-options " -e "
					  (shell-quote-argument regexp)
					  " "
					  (shell-quote-argument "{}")
					  " "
					  ;; Doesn't work with "+".
					  (shell-quote-argument ";"))))

(defun clean-svn (dir buffer)
  "DIR: , BUFFER: ."
  (interactive "DDelete-directory: \nbDelete candidates buffer: ")
  (mapcar (lambda (file) (delete-file (concat dir "/" file)))
		  (split-string
		   (with-current-buffer
			   (get-buffer-create buffer)
			 (buffer-substring-no-properties (point-min) (point-max))) "\n")))

(defun switch-default-dir (dir)
  "DIR: ."
  (interactive "DChoose default directory:")
  (when (not (null dir))
	(setq default-directory dir)))

(defvar *must-loading-files* nil)

(defun ensure-mkdir (dirname)
  "DIRNAME: ."
  (if (not (file-exists-p dirname))
	  (let ((dir (directory-file-name (file-name-directory dirname))))
		(ensure-mkdir dir)))
  (if (not (file-exists-p dirname))
	  (mkdir dirname)))

(defun load-exist-buffer (filename)
  "FILENAME: ."
  (dolist (buffer (buffer-list))
	(with-current-buffer buffer
	  (if (string-equal buffer-file-name filename)
		  (progn
			(wcy-desktop-load-file buffer)
			(cl-return t)))))
  nil)

(defun load-must-files ()
  "."
  (interactive)
  (message "*** load must files...")
  (mapc (lambda (filename)
          (let ((filename (expand-file-name filename)))
            (if (not (load-exist-buffer filename))
                (if (file-exists-p filename)
                    (find-file-noselect filename nil nil nil)
                  (progn
                    (let ((dir (file-name-directory filename)))
                      (ensure-mkdir dir))
                    (with-current-buffer (create-file-buffer filename)
                      (write-file filename)))))))
        *must-loading-files*))

(defun copy-buffer-string ()
  "."
  (interactive)
  (kill-new (buffer-string)))

(defun trim-line-trailing-whitespace ()
  "."
  (interactive)
  (goto-char (line-beginning-position))
  (replace-regexp "\s+\n" "\n"))

(defun trim-trailing-whitespace ()
  "."
  (interactive)
  (save-excursion
    (if (not (region-active-p))
        (trim-line-trailing-whitespace)
      (progn
        (let ((start (region-beginning))
              (end (region-end)))
          (deactivate-mark)
          (message "start: %S, end: %S" start end)
          (progn
            (goto-char end)
            (goto-char (line-end-position))
            (setq end (point)))
          (message "start1: %S, end: %S" start end)
          (progn
            (goto-char start)
            (while (< (point) end)
              (message "start2: %S" (point))
              (trim-line-trailing-whitespace)
              (next-line))))))))

(provide 'utility)
;;; utility.el ends here
