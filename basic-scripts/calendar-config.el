;;; package --- calendar-config.el
;;; Commentary:
;;; Code:

(require 'calendar)
(require 'solar)

(setq calendar-mark-holidays-flag t)
(set-when-non-null calendar-latitude +calendar-latitude+)
(set-when-non-null calendar-longitude +calendar-longitude+)
(set-when-non-null calendar-location-name +calendar-location-name+)
(set-when-non-null calendar-time-zone +calendar-time-zone+)
(set-when-non-null calendar-standard-time-zone-name +calendar-standard-time-zone-name+)
(set-when-non-null calendar-daylight-time-zone-name +calendar-daylight-time-zone-name+)

(setq calendar-christian-all-holidays-flag nil
      calendar-hebrew-all-holidays-flag nil
      calendar-islamic-all-holidays-flag nil
      calendar-bahai-all-holidays-flag nil
      calendar-chinese-all-holidays-flag t)

(defcustom calendar-chinese-celestial-stem
  ["甲" "乙" "丙" "丁" "戊" "己 " "庚" "辛" "壬" "癸"]
  "Prefixes used by `calendar-chinese-sexagesimal-name'."
  :group 'calendar-chinese
  :type '(vector (string :tag "甲")
                 (string :tag "乙")
                 (string :tag "丙")
                 (string :tag "丁")
                 (string :tag "戊")
                 (string :tag "己")
                 (string :tag "庚")
                 (string :tag "辛")
                 (string :tag "壬")
                 (string :tag "癸")))

(defcustom calendar-chinese-terrestrial-branch
  ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"]
  "Suffixes used by `calendar-chinese-sexagesimal-name'."
  :group 'calendar-chinese
  :type '(vector (string :tag "子")
                 (string :tag "丑")
                 (string :tag "寅")
                 (string :tag "卯")
                 (string :tag "辰")
                 (string :tag "巳")
                 (string :tag "午")
                 (string :tag "未")
                 (string :tag "申")
                 (string :tag "酉")
                 (string :tag "戌")
                 (string :tag "亥")))

(defun calendar-chinese-sexagesimal-name (n)
  "The N-th name of the Chinese sexagesimal cycle.
N congruent to 1 gives the first name, N congruent to 2 gives the second name,
..., N congruent to 60 gives the sixtieth name."
  ;; Change "%s-%s" to "%s%s", since adding the extra `-' between two Chinese
  ;; characters looks stupid.
  (format "%s%s"
          (aref calendar-chinese-celestial-stem (% (1- n) 10))
          (aref calendar-chinese-terrestrial-branch (% (1- n) 12))))

(provide 'calendar-config)
;;; calendar-config.el ends here
