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

(defun timestamp-to-string (timestamp)
  "TIMESTAMP: ."
  (interactive "sInput the timestamp: ")
  (let* ((timestamp (if (stringp timestamp) (string-to-number timestamp)
                      timestamp))
         (date-str (format-time-string "%Y-%m-%d %H:%M:%S" timestamp)))
    (message date-str)
    date-str))

(defun org-current-timestamp ()
  "."
  (interactive)
  (let ((ct (format-time-string "%Y-%m-%d %a %H:%M" (current-time))))
    (message ct)
    ct))


(provide 'dates)
;;; dates.el ends here