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
           (camel-string (s-lower-camel-case string)))
      (editors-debug-message camel-string)
      camel-string))))

(defun upper-camel-case ()
  "Replace the string of region with upper camel case format."
  (interactive)
  (cond
   ((region-active-p)
    (let* ((start (region-beginning))
           (end (region-end))
           (region-string (buffer-substring-no-properties start end))
           (string (s-upper-camel-case region-string)))
      (delete-region start end)
      (insert string)))
   (t
    (let* ((string (read-string "Please input string: "))
           (camel-string (s-upper-camel-case string)))
      (editors-debug-message camel-string)
      camel-string))))

(defun s-lower-case (s)
  "Convert S to lowercase; copied from s.el and mofified."
  (declare (side-effect-free t))
  (s-join "" (s--mapcar-head 'downcase 'downcase (s-split-words s))))

(defun lower-case ()
  "Replace the string of region with lower camel case format."
  (interactive)
  (cond
   ((region-active-p)
    (let* ((start (region-beginning))
           (end (region-end))
           (region-string (buffer-substring-no-properties start end))
           (lower-string (s-lower-case region-string)))
      (delete-region start end)
      (insert lower-string)))
   (t
    (let* ((string (read-string "Please input string: "))
           (lower-string (s-lower-case string)))
      (editors-debug-message lower-string)
      lower-string))))

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

(defun open-line-upward ()
  (beginning-of-line)
  (newline-and-indent)
  (forward-line -1)
  (indent-for-tab-command))

(defun previous-line-blank-p ()
  (save-excursion
    (forward-line -1)
    (looking-at-p "^[ \t]*$")))

(defun remove-blank-lines-from-string (input-string)
  "Remove all blank lines from INPUT-STRING and return the result."
  (let ((output-string input-string))
    (setq output-string (replace-regexp-in-string "^[ \t]*\n" "" output-string))
    output-string))

(defun remove-blank-lines-in-region (start end)
  "Remove all blank lines in the region defined by START and END."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (while (re-search-backward "^[ \t]*\n" start t)
      (replace-match ""))))

(defun blank-string-p (input-string)
  "Check if INPUT-STRING is a blank string (only contains whitespace characters)."
  (string-match-p "^[ \t\n]*$" input-string))

(provide 'editors)
;;; editors.el ends here
