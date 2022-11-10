;;; package --- dates.el
;;; Commentary:
;;; Code:


(require 'time-date)

(defconst +date-command+ (if (eq window-system 'ns) "gdate" "date"))

(defun quick-on-content (content)
  "Do some quick actions on CONTENT."
  (cond ((equal current-prefix-arg '(4))
         (message "content: %s" content))
        ((equal current-prefix-arg 1)
         (insert (format "%s" content)))
        ((equal current-prefix-arg 2)
         (kill-new (format "%s" content))
         (message "content: [%s] yanked." content))))

(defun current-timestamp ()
  "."
  (interactive)
  (let ((timestamp (time-convert nil 'integer)))
    (when (called-interactively-p 'any)
      (quick-on-content timestamp))
    timestamp))

(defun tomorrow-timestamp ()
  "."
  (interactive)
  (let ((time (decode-time)))
    (decoded-time--alter-day time t)
    (setf (decoded-time-second time) 0)
    (setf (decoded-time-minute time) 0)
    (setf (decoded-time-hour time) 0)
    (let* ((encoded-time (encode-time time))
           (high (car encoded-time))
           (low (cadr encoded-time))
           (timestamp (+ (ash high 16) low)))
      (when (called-interactively-p 'any)
        (quick-on-content timestamp))
      timestamp)))

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
    (when (called-interactively-p 'any)
      (quick-on-content timestamp))
    timestamp))

(defun system-string-to-timestamp (date-str)
  "DATE-STR: ."
  (interactive "sInput the date string: ")
  (let ((ct (shell-command-to-string (format "%s -d '%s' +%%s" +date-command+ date-str))))
    (message ct)
    ct))


(defun timestamp-to-string-with-format (timestamp time-format)
  "TIMESTAMP: , TIME-FORMAT."
  (let* ((timestamp (if (stringp timestamp) (string-to-number timestamp)
                      timestamp))
         (date-str (format-time-string time-format timestamp)))
    (quick-on-content date-str)
    date-str))

(defun timestamp-to-normal-string (timestamp)
  "TIMESTAMP."
  (interactive "sInput the timestamp: ")
  (timestamp-to-string-with-format timestamp "%Y-%m-%d %H:%M:%S"))

(defun timestamp-to-short-string (timestamp)
  "TIMESTAMP."
  (interactive "sInput the timestamp: ")
  (timestamp-to-string-with-format timestamp "%Y-%m-%d"))

(defun current-time-normal-string ()
  "."
  (interactive)
  (let ((time-string (current-time-string-with-format "%Y-%m-%d %H:%M:%S")))
    time-string))

(defun current-time-short-string ()
  "."
  (interactive)
  (let ((time-string (current-time-string-with-format "%Y-%m-%d")))
    time-string))

(defun current-time-string-with-format (time-format)
  "PREFIX-ARG, TIME-FORMAT."
  (let ((time-string (timestamp-to-string-with-format (current-timestamp) time-format)))
    time-string))

(defun org-current-timestamp ()
  "."
  (interactive)
  (let ((ct (format-time-string "%Y-%m-%d %a %H:%M" (current-time))))
    (message ct)
    ct))

(defun reformat-time-string (time-string timezone-diff-minutes)
  "Reformat TIME-STRING like 2021-10-15T11:39:40.000+00:00 to 2021-10-15 19:39:40 at GMT+8, TIMEZONE-DIFF-MINUTES is for the TIME-STRING."
  (let* ((parsed-date (timezone-parse-date time-string))
         (parsed-time (timezone-parse-time (aref parsed-date 3))))
    (seq-let [year month day _ _] parsed-date
      (seq-let [hour minute second] parsed-time
        ;; (message "%s-%s-%s %s:%s:%s" year month day hour minute second)
        (let* ((decoded-time (list (string-to-number second)
                                   (string-to-number minute)
                                   (string-to-number hour)
                                   (string-to-number day)
                                   (string-to-number month)
                                   (string-to-number year) nil nil timezone-diff-minutes))
               (encoded-time (encode-time decoded-time)))
          ;; (message "decoded time: %s" decoded-time)
          (format-time-string "%Y-%m-%d %H:%M:%S" encoded-time))))))

(provide 'dates)
;;; dates.el ends here
