;;; package --- codings.el
;;; Commentary:
;;; Code:

(require 'unicad)

(setq current-language-environment "UTF-8")
(setq default-input-method "eim-wb")
(setq locale-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setenv "LC_CTYPE" "zh_CN.UTF-8")

(provide 'codings)
;;; codings.el ends here
