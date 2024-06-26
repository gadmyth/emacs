;;; package --- editors.el
;;; Commentary:
;;; Code:

(require 'q)
(require 's)
(require 'clipboard+)

(defvar *wrapper-content*)

(define-debug-message editors)

(defun wrapping (wrapper)
  "WRAPPER: ."
  (let* ((word (word-at-point))
        (import (funcall wrapper word)))
    (setq *wrapper-content* import)
    (message import)))

(defun output-wrapper-content ()
  "."
  (interactive)
  (when *wrapper-content*
    (insert-string *wrapper-content*)
    (setq *wrapper-content* nil)))

(defun wrap-line-with-oc-method ()
  "."
  (interactive)
  (save-excursion
    (re-search-backward "^" nil t)
    (re-search-forward " *" nil t 1)
    (insert "[")
    (re-search-forward "$" nil t 1)
    (insert "];"))
  (end-of-line))

(defun wrap-right-part-with-oc-method ()
  "."
  (interactive)
  (save-excursion
    (end-of-line)
    (re-search-backward "= " nil t)
    (re-search-forward " " nil t 1)
    (insert "[")
    (re-search-forward "$" nil t 1)
    (insert "];"))
  (end-of-line))

(defun edit-left-type ()
  "."
  (interactive)
  (end-of-line)
  (re-search-backward "^" nil t)
  (re-search-forward " *" nil t 1)
  (kill-word 1))

(defun edit-left-arg ()
  "."
  (interactive)
  (end-of-line)
  (re-search-backward "^" nil t)
  (re-search-forward " " nil t 1)
  (kill-word 1))

(defun edit-left-part ()
  "."
  (interactive)
  (end-of-line)
  (re-search-backward "^" nil t)
  (let ((begin (point)))
    (re-search-forward " =" nil t 1)
    (backward-char 2)
    (let ((end (point)))
      (kill-region begin end))))

(defun edit-right-part ()
  "."
  (interactive)
  (beginning-of-line)
  (re-search-forward "= " nil t)
  (let ((begin (point)))
    (re-search-forward "$" nil t 1)
    (let ((end (point)))
      (kill-region begin end))))

(defun edit-value (regex pattern-group)
  "REGEX: , PATTERN-GROUP."
  (re-search-backward "^" nil t)
  (re-search-forward regex nil t 1)
  (goto-char (match-beginning pattern-group))
  (delete-region (match-beginning pattern-group) (match-end pattern-group)))

(defun edit-assign-value ()
  "."
  (interactive)
  (edit-value "^ *?\\([^=]*?\\) = \\(.*\\)$" 2))

(defun edit-line ()
  "."
  (interactive)
  (edit-value "^[ \t]*?\\([^ \t].*\\)$" 1))

(defun edit-css-prop ()
  "."
  (interactive)
  (edit-value "^ *?\\([^:]*?\\) *: *\\(.*?\\)[,;] *?$" 2))

(defun replace-point-word (new-word)
  "NEW-WORD: ."
  (interactive (list (read-string "Replace by: " (car kill-ring))))
  (save-excursion
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (replace-regexp ".*" new-word nil (car bounds) (cdr bounds)))))

(defun replace-point-word-current-line (new-word)
  "NEW-WORD: ."
  (interactive (list (read-string "Replace by: " (car kill-ring))))
  (save-excursion
    (let ((word (word-at-point)))
      (re-search-backward "^" nil t)
      (replace-regexp word new-word nil (line-beginning-position) (line-end-position)))))

(defun lower-camel-case ()
  "Replace the string of region with lower camel case format."
  (interactive)
  (cond
   ((region-active-p)
    (let* ((start (region-beginning))
           (end (region-end))
           (region-string (buffer-substring-no-properties start end))
           (lower-camel-string (s-lower-camel-case region-string)))
      (delete-region start end)
      (insert lower-camel-string)))
   (t
    (let* ((string (read-string "Please input string: "))
           (lower-camel-string (s-lower-camel-case string)))
      (editors-debug-message lower-camel-string)
      lower-camel-string))))

(defun snake-case ()
  "Replace the string of region with snake case format."
  (interactive "r")
  (cond
   ((region-active-p)
    (let* ((start (region-beginning))
           (end (region-end))
           (region-string (buffer-substring-no-properties start end))
           (snake-case-string (s-snake-case region-string)))
      (delete-region start end)
      (insert snake-case-string)))
   (t
    (let* ((string (read-string "Please input string: "))
           (snake-case-string (s-snake-case string)))
      (editors-debug-message snake-case-string)
      snake-case-string))))

(defun dash-case ()
  "Replace the string of region with snake case format."
  (interactive "r")
  (cond
   ((region-active-p)
    (let* ((start (region-beginning))
           (end (region-end))
           (region-string (buffer-substring-no-properties start end))
           (dash-case-string (s-dashed-words region-string)))
      (delete-region start end)
      (insert dash-case-string)))
   (t
    (let* ((string (read-string "Please input string: "))
           (dash-case-string (s-dashed-words string)))
      (message dash-case-string)
      dash-case-string))))


(provide 'editors)
;;; editors.el ends here
