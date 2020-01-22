;;; package --- utility.el
;;; Commentary:
;;; Code:

(require 'package)
(require 'wcy-desktop)

(eval-when-compile (require 'cl))

(defun switch-proxy (enable)
  "ENABLE's value is t or nil."
  (interactive "Senable? ")
  (let ((proxy
		 (if enable
			 "127.0.0.1:8087"
		   nil)))
	(setenv "http_proxy"  proxy)
	(setenv "https_proxy" proxy)))

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

(defvar *must-loading-files*
	  (mapcar (lambda (n) (expand-file-name n))
			  '("~/diary" "~/org/notes.org" "~/org/task.org" "~/org/timeline.org" "~/unix-config/.emacs")))

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
			(return t)))))
  nil)

(defun load-must-files ()
  "."
  (interactive)
  (mapc (lambda (filename)
		  (if (not (load-exist-buffer filename))
			  (if (file-exists-p filename)
				  (find-file-noselect filename nil nil nil)
				(progn
				  (let ((dir (file-name-directory filename)))
					(ensure-mkdir dir))
				  (with-current-buffer (create-file-buffer filename)
					(write-file filename))))))
		*must-loading-files*))

(defun show-symbol-at-point ()
  "."
  (interactive)
  (let* ((sym (symbol-at-point))
         (value (cond ((fboundp sym) 'function)
                      ((boundp sym) (symbol-value sym))
                      (t nil))))
    (message "Symbol: %S [%S]" sym value)))

(defun find-library-at-point ()
  "."
  (interactive)
  (find-library (word-at-point)))

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
