;;; package --- yas-config.el
;;; Commentary:
;;; Code:

(require 'yasnippet)
(require 'ivy)
(require 'dash)
(require 'abbrev)


(setq yas-triggers-in-field t)

(advice-add 'yas-maybe-expand-abbrev-key-filter :filter-return #'yas-maybe-expand-compensate)

(defun yas-maybe-expand-compensate (cmd)
  "When the CMD is nil, return the compensation expand function."
  (or cmd (yas-expand-compensate)))

(defun yas-expand-compensate ()
  "."
  (interactive)
  (if (not (expand-abbrev))
      (comint-dynamic-complete-filename))
  ;; return nil forever
  nil)

(defvar *yas-snippet-extra-dirs* nil)

(defun yas-add-extra-dirs ()
  (when (bound-and-true-p *yas-snippet-extra-dirs*)
    (dolist (d *yas-snippet-extra-dirs*)
      (let ((dir (expand-file-name d)))
        (when (file-directory-p dir)
          (add-to-list 'yas-snippet-dirs dir))))))

(add-hook 'yas-global-mode-hook #'yas-add-extra-dirs)

(setq yas-indent-line 'fixed)

;; define a global key for list and expand snippet for current major mode
(global-set-key (kbd "C-x x") 'expand-snippet-for-current-mode)

;; Override the snippet-mode-map's key
(define-key snippet-mode-map "\C-c\C-c" #'yas-save-new-snippet-buffer)

(defun yasnippet-list-dir (&optional action)
  "ACTION."
  (interactive)
  (ivy-read "Choose yasnippet dir: " (yas-snippet-dirs)
            :action action))

(defun yasnippet-goto-dir ()
  "."
  (interactive)
  (yasnippet-list-dir #'(lambda (dir)
                          (dired dir))))

(defun yasnippet-major-mode-goto-dir ()
  "."
  (interactive)
  (yasnippet-list-dir #'(lambda (dir)
                          (dired (format "%s/%s" dir major-mode)))))

(defun yasnippet-filter-major-mode-dir ()
  "."
  (interactive)
  (ivy-read "Choose yasnippet dir: " (remove-if-not (lambda (dir) (file-exists-p (format "%s/%s" dir major-mode))) (yas-snippet-dirs))
            :action (lambda (dir)
                      (dired (format "%s/%s" dir major-mode)))))

(defun yasnippet-all-defined-modes ()
  "."
  (let ((modes))
    (dolist (dir (yas-snippet-dirs))
      (when (and (file-directory-p dir))
        (let ((files (directory-files dir)))
          (dolist (file-name files)
            (let ((whole-path-file (expand-file-name file-name dir)))
              (when (and
                     (not (string-equal "." file-name))
                     (not (string-equal ".." file-name))
                     (file-directory-p whole-path-file)
                     (not (-contains-p modes file-name)))
                (push file-name modes)))))))
    modes))

(yasnippet-all-defined-modes)

(defun expand-snippet-for-mode ()
  "List snippets defined for mode, and expand the selected snippet, default mode is major mode."
  (interactive)
  (ivy-read
   "target mode for snippet: " (reverse (yasnippet-all-defined-modes))
   :preselect (symbol-name major-mode) :action
   #'expand-snippet-for-mode-internal))

(defun expand-snippet-for-current-mode ()
  "Expand the selected snippet of listing snippets for current major mode."
  (interactive)
  (expand-snippet-for-mode-internal major-mode))

(defun select-snippet-for-mode (mode action)
  "List snippets for MODE, and do something with ACTION."
  (let* ((yas-buffer-local-condition 'always)
         (templates (yas--all-templates (yas--get-snippet-tables mode)))
         (template (and templates
                        (or (yas--prompt-for-template
                             templates
                             "Choose a snippet template to expand: ")
                            (car templates))))
         (name (and template (yas--template-name template)))
         (snippet (and name (yas-lookup-snippet name mode))))
    (when action
      (funcall action snippet))))

(defun expand-snippet-for-mode-internal (mode)
  "List snippets defined for MODE, and expand the selected snippet, default mode is major mode."
  (select-snippet-for-mode mode
                           (lambda (snippet)
                             (if snippet (yas-expand-snippet snippet)
                               (message "No snippets tables active!")))))

(defun yas-create-snippet-with-region (file-name snippet-name key)
  "FILE-NAME, SNIPPET-NAME, KEY."
  (interactive "sSnippet File Name: \nsSnippet Name: \nsSnippet Key: ")
  (when (region-active-p)
    (let* ((start (region-beginning))
           (end (region-end))
           (content (buffer-substring start end)))
      (yas-new-snippet)
      (setq-local snippet-file-name file-name)
      (insert snippet-name)
      (yas-next-field)
      (insert key)
      (yas-next-field))))

(defun yas-save-new-snippet-buffer ()
  "Save +new-snippet+ buffer or buffer opened by yas-visit-snippet-file command."
  (interactive)
  (let* ((buffer (get-buffer yas-new-snippet-buffer-name))
         (current-buffer (current-buffer))
         (current-buffer-major-mode (with-current-buffer current-buffer major-mode)))
    (if (and (null buffer)
             (equal 'snippet-mode current-buffer-major-mode))
        (setq buffer current-buffer))
    (message "save-new-snippet-buffer, buffer: %S" buffer)
    (when (buffer-live-p buffer)
      (switch-to-buffer buffer)
      (yasnippet-list-dir
       #'(lambda (dir)
           (let* ((table (yas--read-table))
                  (default-directory (format "%s/%s" dir table))
                  (template (yas-load-snippet-buffer table t))
                  (default-file-name (or snippet-file-name
                                         (yas--template-name template))))
             (rename-buffer default-file-name t)
             (save-buffer)
             (quit-window t)))))))

(defun yas-expand-snippet-with-callback-and-params (snippet-name callback &rest params)
  "SNIPPET-NAME, CALLBACK, PARAMS."
  (interactive)
  (when-let ((snippet (yas-lookup-snippet snippet-name)))
    (yas-expand-snippet snippet)
    (let ((index 1)
          (length (length params))
          (snippet))
      (dolist (p params)
        (unless (or (string-equal "__default__" p)
                    (string-equal "" p))
          (insert p))
        ;; before last time yas-next-field (still having active snippets), call the callback function
        (when (and (= index length) callback)
          (setq snippet (car (yas-active-snippets))))
        (yas-next-field)
        (when snippet
          (funcall callback snippet))
        (setq index (1+ index))))
    (yas-exit-all-snippets)))

(defun indent-expanded-snippet (snippet)
  "Get the start and end positions of the last expanded SNIPPET."
  (when snippet
    (let* ((overlay (yas--snippet-control-overlay snippet))
           (start (overlay-start overlay))
           (end (overlay-end overlay)))
      (indent-region start end))))

(defmacro yas-expand-snippet-with-params (snippet-name &rest params)
  `(yas-expand-snippet-with-callback-and-params ,snippet-name nil ,@params))

(defmacro yas-expand-and-indent-snippet-with-params (snippet-name &rest params)
  `(yas-expand-snippet-with-callback-and-params ,snippet-name #'indent-expanded-snippet ,@params))

(defun yas-expand-snippet-with-region ()
  "."
  (interactive)
  (when (region-active-p)
    (let* ((start (region-beginning))
           (end (region-end))
           (content (buffer-substring start end))
           (params (split-string content)))
      (select-snippet-for-mode major-mode
                               (lambda (snippet)
                                 (eval `(yas-expand-snippet-with-params nil (yas--template-name snippet) ,@params)))))))

(provide 'yas-config)
;;; yas-config.el ends here
