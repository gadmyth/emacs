;;; package --- highlights.el
;;; Commentary:
;;; Code:

(defun highlight-current-line ()
  "Highlight the current line using an overlay."
  (interactive)
  (let ((line-start (line-beginning-position))
        (line-end (line-end-position)))
    (let ((overlay (make-overlay line-start line-end)))
      (overlay-put overlay 'face '(:background "yellow"))
      ;; 可选：设置 overlay 的持续时间
      (overlay-put overlay 'evaporate t))))

(defun dehighlight-current-line ()
  "Remove all overlays from the current line."
  (interactive)
  (let ((line-start (line-beginning-position))
        (line-end (line-end-position)))
    (remove-overlays line-start line-end)))

(provide 'highlights)
;;; highlights.el ends here
