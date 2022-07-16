;;; package --- youdao-dictionary-conf.el
;;; Commentary:
;;; Code:

(require-package
 'youdao-dictionary
 (global-set-key (kbd "\C-c t") #'youdao-dictionary-search-at-point-tooltip))

(provide 'youdao-dictionary-conf)
;;; youdao-dictionary-conf.el ends here
