;;; package --- dates.el
;;; Commentary:
;;; Code:

(defconst +date-command+ (if (eq window-system 'ns) "gdate" "date"))

(defun current-timestamp ()
  "."
  (interactive)
  (let ((timestamp (time-convert nil 'integer)))
    (cond ((equal current-prefix-arg '(4))
           (message "current-timestamp: %S" timestamp))
          ((eq current-prefix-arg 3)
           (kill-new (number-to-string timestamp))
           (message "current-timestamp: [%S] yanked." timestamp))
          )
    timestamp))

(defun system-current-timestamp ()
  "."
  (interactive)
  (let* ((ct (shell-command-to-string (format "%s +%%s" +date-command+)))
         (ct (substring ct 0 (- (length ct) 1))))
    (message ct)
    ct))

(defun string-to-timestamp (date-str)
  "DATE-STR: ."
  (interactive "sInput the date string: ")
  (let* ((list-time (parse-time-string date-str))
         (emacs-time (encode-time list-time))
         (timestamp (time-convert emacs-time 'integer)))
    (cond ((equal current-prefix-arg '(4))
           (message "timestamp parsed from %s: [%S]" date-str timestamp))
          ((eq current-prefix-arg 3)
           (kill-new (number-to-string timestamp))
           (message "current-timestamp: [%S] yanked." timestamp))
          )
    timestamp))

(defun system-string-to-timestamp (date-str)
  "DATE-STR: ."
  (interactive "sInput the date string: ")
  (let ((ct (shell-command-to-string (format "%s -d '%s' +%%s" +date-command+ date-str))))
    (message ct)
    ct))


(defun timestamp-to-string-with-format (timestamp time-format)
  "TIMESTAMP: , TIME-FORMAT."
  (interactive "sInput the timestamp: ")
  (let* ((timestamp (if (stringp timestamp) (string-to-number timestamp)
                      timestamp))
         (date-str (format-time-string time-format timestamp)))
    date-str))

(defun current-time-normal-string ()
  "."
  (interactive)
  (current-time-string-with-format current-prefix-arg "%Y-%m-%d %H:%M:%S"))

(defun current-time-short-string ()
  "."
  (interactive)
  (current-time-string-with-format current-prefix-arg "%Y-%m-%d"))

(defun current-time-string-with-format (prefix-arg time-format)
  "PREFIX-ARG, TIME-FORMAT."
  (let ((time-string (timestamp-to-string-with-format (current-timestamp) time-format)))
    (when prefix-arg
      (cond ((equal prefix-arg '(4))
             (message "current time normal string is: [%s]" time-string))
            ((eq prefix-arg 3)
             (kill-new time-string)
             (message "current time normal string: [%S] yanked." time-string))
            ((eq prefix-arg 8)
             (insert time-string))
            ))
    time-string))

(defun org-current-timestamp ()
  "."
  (interactive)
  (let ((ct (format-time-string "%Y-%m-%d %a %H:%M" (current-time))))
    (message ct)
    ct))



(provide 'dates)
;;; dates.el ends here
