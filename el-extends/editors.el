;;; package --- editors.el
;;; Commentary:
;;; Code:

(require 's)

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

(defun split-string-of-region (start end)
  "Format and insert sql IN collection from START to END of region."
  (interactive "r")
  (when (region-active-p)
    (let* ((region-string (buffer-substring-no-properties start end))
           (seperator (read-string "Please input the seperator: "))
           (lines (split-string region-string seperator)))
      (goto-char end)
      (insert "\n")
      (seq-doseq (line lines)
        (seq-doseq (column (split-string line seperator))
          (let ((column (s-trim column)))
            (when (> (length column) 0)
              (insert (s-trim column) "\n"))))))))

(defun count-line-of-region (start end)
  "Format and insert sql IN collection from START to END of region."
  (interactive "r")
  (when (region-active-p)
    (let* ((region-string (buffer-substring-no-properties start end))
           (seperator (read-string "Please input the seperator: "))
           (lines (split-string region-string seperator)))
      (message "line count is %d" (length lines)))))

(defun cut-string-of-region (start end)
  "Format and insert sql IN collection from START to END of region."
  (interactive "r")
  (when (region-active-p)
    (let* ((region-string (buffer-substring-no-properties start end))
           (lines (split-string region-string "\n"))
           (column-index (read-number "Please input the cut column index: ")))
      (goto-char end)
      (insert "\n")
      (seq-doseq (line lines)
        (insert (nth column-index (split-string line)) "\n")))))

(defun join-to-string-list (start end)
  "Join list to a string list from START to END of region."
  (interactive "r")
  (when (region-active-p)
    (let* ((region-string (buffer-substring-no-properties start end))
           (lines (split-string region-string "\n"))
           (lines-with-action (mapcar (lambda (str) (format "\"%s\"" str)) lines)))
      (goto-char end)
      (insert "\n" "(" (string-join lines-with-action " ") ")" "\n"))))

(defun join-with-comma ()
  "Format and insert sql IN collection from START to END of region."
  (interactive)
  (cond
   ((region-active-p)
    (let* ((start (region-beginning))
           (end (region-end))
           (region-string (buffer-substring-no-properties start end))
           (lines (split-string region-string "\n"))
           (lines-with-action lines))
      (goto-char end)
      (insert "\n" (string-join lines-with-action ",") "\n")))
   (t
    (let* ((string (read-string "Please input string: "))
           (joined-string (string-join (split-string string "\n") ",")))
      (message joined-string)
      joined-string))))

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
      (message lower-camel-string)
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
      (message snake-case-string)
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

(defun join-the-line ()
  "."
  (interactive)
  (let* ((ev last-command-event)
         (base (event-basic-type ev)))
    (pcase base
      (?\j (forward-line)
           (join-line))
      (?\k (join-line))
      ('up (forward-line -1))
      ('down (forward-line))
      (?\/ (undo))
      (_ nil)))
  (message "Use j to join the next line, use k join the previous line")
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (vector (list ?\j))
       (lambda () (interactive) (join-the-line)))
     (define-key map (vector (list ?\k))
       (lambda () (interactive) (join-the-line)))
     (define-key map (kbd "<up>")
       (lambda () (interactive) (join-the-line)))
     (define-key map (kbd "<down>")
       (lambda () (interactive) (join-the-line)))
     (define-key map (vector (append '(control) (list ?\/)))
       (lambda () (interactive) (join-the-line)))
     map)))

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

(defun kill-the-whole-line-ring-save ()
  "START, END."
  (interactive)
  (let* ((region-active-p (region-active-p))
         (text-beg (if region-active-p (region-beginning) (line-beginning-position)))
         (text-end (if region-active-p (region-end) (line-end-position))))
    (cond
     ((> text-end text-beg)
      (cond
       ((executable-find "xclip")
        (let* ((content (buffer-substring-no-properties text-beg text-end))
               (mktemp-cmd (executable-find "mktemp"))
               (tmp-file-path (shell-command-to-string (format "%s /tmp/emacs.king-ring.XXXX" mktemp-cmd)))
               (tmp-file-path (replace-regexp-in-string "\n" "" tmp-file-path))
               (xclip-cmd (executable-find "xclip"))
               (command (format "cat %s | xclip -sel c" tmp-file-path xclip-cmd)))
          (message "content: %s" content)
          (message "temp file: %s" tmp-file-path)
          (message "command: %s" command)
          (write-region content nil tmp-file-path)
          (call-process-shell-command command)
          (delete-file tmp-file-path)
          (deactivate-mark)))
       (t
        (kill-ring-save text-beg text-end)))
      (if region-active-p
          (message "*** region copied ***")
        (message "*** line copied ***")))
     (t
      (message "Can't copy empty content!")
      (if region-active-p
          (deactivate-mark))))))

(defun mark-the-whole-line ()
  "."
  (interactive)
  (beginning-of-visual-line)
  (push-mark-command nil)
  (end-of-visual-line))

(defun remove-all-text-properties-region (beg end)
  "Remove all the text properties with region from BEG to END."
  (interactive "r")
  (when (region-active-p)
    (set-text-properties beg end nil)))

;; ----- global keys for line editor -----

;; C-k: kill-line from current point to end of line
;; C-S-u: kill-to-beginning-of-line
;; C-S-k: kill the whole line
;; M-w origin key bind to kill-ring-save, original function is copy region

(global-set-key (kbd "C-S-u") 'kill-to-beginning-of-line)
(global-set-key (kbd "C-S-k") 'kill-the-whole-line)
(global-set-key (kbd "M-w") 'kill-the-whole-line-ring-save)
(global-set-key (kbd "C-S-l") 'mark-the-whole-line)

;; define the *global-join-map*
(defvar *global-join-map* (make-sparse-keymap))
(define-key (current-global-map) (kbd "C-x j j") 'join-the-line)
(define-key (current-global-map) (kbd "C-x j k") 'join-the-line)

(provide 'editors)
;;; editors.el ends here
