;;; package --- lines.el
;;; Commentary:
;;; Code:

(require-package 'q)

(define-debug-message lines)

(defvar *join-line-seperator* " ")

(defun reset-join-line-seperator ()
  (interactive)
  (setq *join-line-seperator* ?\s))

(defun set-join-line-seperator (seperator)
  (interactive "sPlease input the seperator: ")
  (setq *join-line-seperator* seperator))

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
      (lines-debug-message joined-string)
      joined-string))))

(defun sql-in-collection (start end)
  "Format and insert sql IN collection from START to END of region."
  (interactive "r")
  (when (region-active-p)
    (let* ((region-string (buffer-substring-no-properties start end))
           (lines (split-string region-string "\n"))
           (lines-with-action (mapcar (lambda (str) (format "'%s'" str)) lines)))
      (goto-char end)
      (insert "\n" "(" (string-join lines-with-action ",") ")" "\n"))))

(defun join-to-string-list (start end)
  "Join list to a string list from START to END of region."
  (interactive "r")
  (when (region-active-p)
    (let* ((region-string (buffer-substring-no-properties start end))
           (lines (split-string region-string "\n"))
           (lines-with-action (mapcar (lambda (str) (format "\"%s\"" str)) lines)))
      (goto-char end)
      (insert "\n" "(" (string-join lines-with-action " ") ")" "\n"))))

(defun join-line-replace-seperator ()
  "."
  (cond
   ((eq *join-line-seperator* " ")
    ;; do nothing
    t)
   ((eq *join-line-seperator* "")
    (delete-char 1))
   (t
    (delete-char 1)
    (insert *join-line-seperator*))))

(defun join-the-line ()
  "."
  (interactive)
  (let* ((ev last-command-event)
         (base (event-basic-type ev)))
    (pcase base
      (?\j (forward-line)
           (join-line)
           (join-line-replace-seperator))
      (?\k (join-line)
           (join-line-replace-seperator))
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

(defun current-line-contains-regexp (regexp)
  "Check if the current line contains a match for the specified REGEXP."
  (save-excursion
    (beginning-of-line)
    (re-search-forward regexp (line-end-position) t)))

;; define the *global-join-map*
(defvar *global-join-map* (make-sparse-keymap))
(define-key (current-global-map) (kbd "C-x j j") 'join-the-line)
(define-key (current-global-map) (kbd "C-x j k") 'join-the-line)

(provide 'lines)
;;; join.el ends here
