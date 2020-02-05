;;; package --- holiday-config.el
;;; Commentary:
;;; Code:

(require 'calendar)

(setq calendar-mark-holidays-flag t)

(setq calendar-christian-all-holidays-flag nil
      calendar-hebrew-all-holidays-flag nil
      calendar-islamic-all-holidays-flag nil
      calendar-bahai-all-holidays-flag nil
      calendar-chinese-all-holidays-flag t)

(provide 'holiday-config)
;;; holiday-config.el ends here
