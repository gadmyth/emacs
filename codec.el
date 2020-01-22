;;; package --- codec.el
;;; Commentary:
;;; Code:

(defun url-encode-region ()
  "."
  (interactive)
  (let ((url-str (delete-and-extract-region (region-beginning) (region-end))))
    (insert (url-hexify-string url-str))))

(defun url-decode-region ()
  "."
  (interactive)
  (let ((url-str (delete-and-extract-region (region-beginning) (region-end))))
    (insert (decode-coding-string (url-unhex-string url-str) 'utf-8))))

(defun md5-encode-region ()
  "."
  (interactive)
  (let ((str (delete-and-extract-region (region-beginning) (region-end))))
    (insert (md5 str))))

(defun encode-file-as-base64-uri (file)
  "FILE is the png or jpeg to encode."
  (interactive "ffile: ")
  (let* ((buffer-content (with-temp-buffer (insert-file-contents file) (buffer-string)))
         (base64-content (base64-encode-string buffer-content))
         (uri (concat "data:" "image/png" ";base64," base64-content)))
    (message "encode-file-as-base-64-uri: %s" uri)
    uri))

(provide 'codec)
;;; codec.el ends here