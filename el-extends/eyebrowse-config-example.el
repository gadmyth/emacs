;;; package --- eyebrowse-config-example.el
;;; Commentary:
;;; Code:

(require 'eyebrowse-config)

(eyebrowse-rename-window-config 1 "java")

(dolist (tag '("org" "emacs" "misc"))
  (eyebrowse-create-window-config-with-tag tag))

(provide 'eyebrowse-config-example)
;;; eyebrowse-config-example.el ends here
