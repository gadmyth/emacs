;;; autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "basic-config" "basic-config.el" (0 0 0 0))
;;; Generated autoloads from basic-config.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "basic-config" '("yes-or-no-p")))

;;;***

;;;### (autoloads nil "customized-dir" "customized-dir.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from customized-dir.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "customized-dir" '("customized-dir" "remove-customized-dir" "add-customized-dir" "switch-" "save-customized-dir" "load-customized-dir" "*customized-dir*" "+customized-dir-file-name+" "+dired-al-mode-header+" "new-buffer")))

;;;***

;;;### (autoloads nil "dates" "dates.el" (0 0 0 0))
;;; Generated autoloads from dates.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dates" '("org-current-timestamp" "timestamp-to-string" "string-to-timestamp" "current-timestamp" "+date-command+")))

;;;***

;;;### (autoloads nil "ediff-trees" "ediff-trees.el" (22289 62055
;;;;;;  0 0))
;;; Generated autoloads from ediff-trees.el

(autoload 'ediff-trees "ediff-trees" "\
Starts a new ediff session that recursively compares two
trees.

\(fn ROOT1 ROOT2)" t nil)

;;;***

;;;### (autoloads nil "el-server" "el-server.el" (0 0 0 0))
;;; Generated autoloads from el-server.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "el-server" '("my-" "*my-default-elnode-" "reset-my-default-elnode-url-mapping-table" "elnode-")))

;;;***

;;;### (autoloads nil "eyebrowse-config" "eyebrowse-config.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from eyebrowse-config.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "eyebrowse-config" '("eyebrowse-" "ivy-eyebrowse-config-s")))

;;;***

;;;### (autoloads nil "frames" "frames.el" (0 0 0 0))
;;; Generated autoloads from frames.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "frames" '("set-suitable-frame-size" "*max-frame-")))

;;;***

;;;### (autoloads nil "org-config" "org-config.el" (0 0 0 0))
;;; Generated autoloads from org-config.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-config" '("org-publish-current-file-without-inner-resource-files" "m/org-html-checkbox" "*org-export-res-files*")))

;;;***

;;;### (autoloads nil "packages" "packages.el" (0 0 0 0))
;;; Generated autoloads from packages.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "packages" '("require" "install-packages-if-needed" "*sync-package*" "option-packages" "tool-packages" "package-host" "add-to-list-if-not-exist")))

;;;***

;;;### (autoloads nil "pretty-mode+" "pretty-mode+.el" (22289 62055
;;;;;;  0 0))
;;; Generated autoloads from pretty-mode+.el

(autoload 'pretty-mode+ "pretty-mode+" "\


\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "redis-config" "redis-config.el" (0 0 0 0))
;;; Generated autoloads from redis-config.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "redis-config" '("r-")))

;;;***

;;;### (autoloads nil "utility" "utility.el" (0 0 0 0))
;;; Generated autoloads from utility.el

(autoload 'install-package "utility" "\
PACKAGE is package name; MIN-VERSION is min version of package; NO-REFRESH is whether to refresh contents.

\(fn PACKAGE &optional MIN-VERSION NO-REFRESH)" nil nil)

(autoload 'switch-proxy "utility" "\
ENABLE's value is t or nil.

\(fn ENABLE)" t nil)

(autoload 'find2-grep-dired "utility" "\
DIR is the root directory of find command, REGEXP is the file's regular expression.

\(fn DIR REGEXP)" t nil)

(autoload 'clean-svn "utility" "\


\(fn DIR BUFFER)" t nil)

(autoload 'switch-default-dir "utility" "\


\(fn DIR)" t nil)

(autoload 'load-must-files "utility" "\
.

\(fn)" t nil)

(autoload 'show-symbol-at-point "utility" "\
.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "utility" '("find-library-at-point" "load-exist-buffer" "ensure-mkdir" "*find-grep-dired--dir*" "*must-loading-files*" "get-workspace" "goto-" "swap-to-main-window")))

;;;***

;;;### (autoloads nil "wcy-desktop" "wcy-desktop.el" (22289 62055
;;;;;;  0 0))
;;; Generated autoloads from wcy-desktop.el

(defvar wcy-desktop-file-name "~/.wcy_desktop_save")

;;;***

;;;### (autoloads nil "windows" "windows.el" (0 0 0 0))
;;; Generated autoloads from windows.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "windows" '("quit-help-window")))

;;;***

;;;### (autoloads nil nil ("abbrev-config.el" "annot.el" "anythings.el"
;;;;;;  "avy-config.el" "clipboard.el" "codec.el" "codings.el" "dired++.el"
;;;;;;  "emoji-config.el" "evil-config.el" "eww-config.el" "files-config.el"
;;;;;;  "fonts.el" "holiday-config.el" "ido-config.el" "irc-config.el"
;;;;;;  "key-bindings.el" "linum-relative.el" "lisping-snippet.el"
;;;;;;  "mail-config.el" "mimes.el" "mode-bars.el" "network-util.el"
;;;;;;  "projectile-config.el" "python-config.el" "rsa.el" "scales.el"
;;;;;;  "script-extends.el" "servers.el" "shells.el" "slime-config.el"
;;;;;;  "smart-compile-config.el" "swiper-config.el" "tab-config.el"
;;;;;;  "themes.el" "translate.el" "unicad.el" "uniquify-config.el"
;;;;;;  "version-controll.el" "web-config.el" "workspace.el" "yas-config.el")
;;;;;;  (0 0 0 0))

;;;***

(provide 'autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; autoloads.el ends here
