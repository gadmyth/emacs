;;; package --- buffers.el
;;; Commentary:
;;; Code:


(global-display-line-numbers-mode 0)
(global-visual-line-mode t)
(global-auto-revert-mode t)

(show-paren-mode 1)
(setq column-number-mode t)
(setq line-number-mode t)
(setq size-indication-mode t)

(defun new-buffer (name)
  "NAME: ."
  (interactive (list (read-string "Create buffer(*scratch*): " nil nil "*scratch*")))
  (let ((buffer (generate-new-buffer name)))
    (switch-to-buffer buffer)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))

(defvar *quick-buffer-prefix-list* '("*scratch*" "*Messages*" "*Help*" "*terminal" "*eshell*" "*shell*"))

(defun switch-scratch-buffers (&rest _)
  "NAME: ."
  (interactive)
  (let* ((quick-buffers (seq-filter (lambda (buffer)
                                      (let ((matched-list (mapcar
                                                           (lambda (name)
                                                             (string-prefix-p name (buffer-name buffer)))
                                                           *quick-buffer-prefix-list*)))
                                        (eval `(or ,@matched-list))))
                                    (buffer-list)))
         (quick-buffers (sort (mapcar #'buffer-name quick-buffers) 'string<))
         (quick-buffers (cons "*scratch*" (delete "*scratch*" quick-buffers)))
         (quick-buffer-names (mapcar (lambda (bname)
                                       (let* ((buffer (get-buffer bname))
                                              (mode (with-current-buffer buffer major-mode))
                                              (rich-name bname))
                                         (when (or (eq 'term-mode mode)
                                                   (eq 'shell-mode mode)
                                                   (eq 'eshell-mode mode))
                                           (setq rich-name (format "%s (%s)" bname (with-current-buffer buffer default-directory))))
                                         (cons rich-name bname)))
                                     quick-buffers))
         (default (if (> (length quick-buffer-names) 0) (car (car quick-buffer-names)) nil))
         (name (completing-read "Switch to buffer: " quick-buffer-names nil t nil nil default))
         (buffer (get-buffer (assoc-default name quick-buffer-names #'string=))))
    (if (buffer-live-p buffer)
        (progn
          (switch-to-buffer buffer)
          (setq buffer-offer-save t))
      (message (format "The buffer %s doesn't exist!" name)))))

(defun guess-buffer-major-mode (buffer-name)
  "Return the major mode associated with the given file SUFFIX from auto-mode-alist."
  (let ((mode)
        (case-fold-search))
    (catch 'break
      (dolist (entry auto-mode-alist mode)
        (when (string-match (car entry) buffer-name)
          (setq mode (cdr entry))
          (throw 'break mode))))))

(defmacro operate-on-file-buffer (filename &rest body)
  "Check if FILENAME is already open. If it is, operate on its buffer.
If it is not, prompt to open the file and then operate on its buffer."
  `(let ((buffer (get-file-buffer ,filename)))
     (unless buffer
       (when (y-or-n-p (format "File %s is not open. Open it? " ,filename))
         (setq buffer (find-file ,filename))))
     (when buffer
       (message "File %s is already open. Performing operations in its buffer." ,filename)
       (with-current-buffer buffer
         (let ((guess-major-mode (guess-buffer-major-mode (buffer-name buffer))))
           (unless (eq major-mode guess-major-mode)
             (revert-buffer))
           ,@body)))))

(global-set-key (kbd "s-<left>") #'previous-buffer)
(global-set-key (kbd "s-<right>") #'next-buffer)

(provide 'buffers)
;;; buffers.el ends here
