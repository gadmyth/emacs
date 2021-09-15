;;; package --- youdao-dictionary-conf.el
;;; Commentary:
;;; Code:

(require-safely
 'youdao-dictionary
 (global-set-key (kbd "\C-c t") #'youdao-dictionary-search-at-point-posframe))

(provide 'youdao-dictionary-conf)
;;; youdao-dictionary-conf.el ends here
