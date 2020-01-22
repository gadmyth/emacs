;;; package --- yas-config.el
;;; Commentary:
;;; Code:

(require 'yasnippet)
(add-hook
 'yas-global-mode-hook
 (lambda ()
   (add-to-list 'yas-snippet-dirs (expand-file-name "~/snippets"))
   (add-to-list 'yas-snippet-dirs (expand-file-name "~/emacs/snippets"))))

(setq yas-indent-line 'fixed)

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

(defun yas-create-snippet-with-region (name key)
  "NAME, KEY."
  (interactive "sSnippet Name: \nsSnippet Key: ")
  (when (region-active-p)
     (let* ((start (region-beginning))
            (end (region-end))
            (content (buffer-substring start end)))
         (yas-new-snippet)
         (insert name)
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
      (yasnippet-list-dir #'(lambda (dir)
                              (let* ((table (yas--read-table))
                                     (default-directory (format "%s/%s" dir table))
                                     (template (yas-load-snippet-buffer table t))
                                     (default-file-name (yas--template-name template)))
                                (rename-buffer default-file-name t)
                                (save-buffer)
                                (quit-window t)))))))

(provide 'yas-config)
;;; yas-config.el ends here
