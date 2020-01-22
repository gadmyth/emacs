(require 'google-translate)
(setq-default google-translate-enable-ido-completion t)
(setq-default google-translate-default-source-language "en")
(setq-default google-translate-default-target-language "zh_CN")

(global-set-key (kbd "C-x c t") 'google-translate-at-point)

(provide 'translate)
