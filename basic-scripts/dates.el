;;; package --- dates.el
;;; Commentary:
;;; Code:

(defconst +date-command+ (if (eq window-system 'ns) "gdate" "date"))

(defun current-timestamp ()
  "."
  (interactive)
  (let* ((ct (shell-command-to-string (format "%s +%%s" +date-command+)))
         (ct (substring ct 0 (- (length ct) 1))))
    (message ct)
    ct))

(defun string-to-timestamp (date-str)
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
    (message date-str)
    date-str))

(defun timestamp-to-string (timestamp)
  ".TIMESTAMP: ."
  (timestamp-to-string-with-format timestamp "%Y-%m-%d %H:%M:%S"))

(defun timestamp-to-short-string (timestamp)
  ".TIMESTAMP: ."
  (timestamp-to-string-with-format timestamp "%Y-%m-%d"))

(defun show-current-time-string ()
  "."
  (interactive)
  (timestamp-to-string (current-timestamp)))

(defun org-current-timestamp ()
  "."
  (interactive)
  (let ((ct (format-time-string "%Y-%m-%d %a %H:%M" (current-time))))
    (message ct)
    ct))

(defun insert-current-time ()
  "."
  (interactive)
  (insert (current-time-string)))

(defun insert-date-normal-string ()
  "."
  (interactive)
  (insert (timestamp-to-string (current-timestamp))))

(defun insert-date-normal-short-string ()
  "."
  (interactive)
  (insert (timestamp-to-short-string (current-timestamp))))


(provide 'dates)
;;; dates.el ends here
