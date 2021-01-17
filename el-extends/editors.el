;;; package --- editors.el
;;; Commentary:
;;; Code:

(defvar *wrapper-content*)

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

(defun sql-in-collection (start end)
  "Format and insert sql IN collection from START to END of region."
  (interactive "r")
  (when (region-active-p)
    (let* ((region-string (buffer-substring-no-properties start end))
           (lines (split-string region-string "\n"))
           (lines-with-action (mapcar (lambda (str) (format "'%s'" str)) lines)))
      (goto-char end)
      (insert "\n" "(" (string-join lines-with-action ",") ")" "\n"))))

;; ------ line editor -------

(defun kill-to-beginning-of-line ()
  "."
  (interactive)
  (save-excursion
    (let ((end (point))
          (begin (progn
                   (beginning-of-visual-line)
                   (point))))
    (kill-region begin end))))

(defun kill-the-whole-line ()
  "."
  (interactive)
  (save-excursion
    (let ((begin (progn
                   (beginning-of-visual-line)
                   (point)))
          (end (progn
                 (end-of-visual-line)
                 (point))))
      (kill-region begin end))))

(defun kill-the-whole-line-ring-save (start end)
  "START, END."
  (interactive "r")
  (if (region-active-p)
      (kill-ring-save start end)
    (save-excursion
      (let ((begin (progn
                     (beginning-of-visual-line)
                     (point)))
            (end (progn
                   (end-of-visual-line)
                   (point))))
        (kill-ring-save begin end)
        (message "*** line copied ***")))))

;; ----- global keys for line editor -----

;; C-k: kill-line from current point to end of line
;; C-S-u: kill-to-beginning-of-line
;; C-S-k: kill the whole line
;; M-w origin key bind to kill-ring-save, original function is copy region

(global-set-key (kbd "C-S-u") 'kill-to-beginning-of-line)
(global-set-key (kbd "C-S-k") 'kill-the-whole-line)
(global-set-key (kbd "M-w") 'kill-the-whole-line-ring-save)

(provide 'editors)
;;; editors.el ends here
