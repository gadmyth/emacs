;;; package --- abbrev-config.el
;;; Commentary:
;;; Code:

(setq default-abbrev-mode t)
(setq save-abbrevs t)

(require 'yasnippet)
(defadvice yas-expand (around expand-abbrev-when-word-p)
  (interactive)
  (let ((word (thing-at-point 'word t)))
    (when word
      (expand-abbrev)))
  ad-do-it)
(ad-activate 'yas-expand)

;; for objc-mode
(add-hook
 'objc-mode-hook
 (lambda ()
   (progn
     (mapc
      (lambda (pair)
        (define-abbrev objc-mode-abbrev-table (car pair) (cdr pair)))
      '(("cd" . "class-def")
        ("ced" . "class-ext-def")
        ("ci" . "class-impl")
        ("ced" . "class-ext-def")
        ("cei" . "class-ext-impl")
        ("pd" . "prop-def")
        ("p2d" . "prop2-def")
        ("pbd" . "prop-block-def")
        ("ai" . "allocinit")
        ("initd" . "init-demo")
        ("cd" . "class-def")
        ("fd" . "func-def")
        ("fd2" . "func2-def")
        ("gfd" . "gfunc-def")
        ("bd" . "block-declare")
        ("bi" . "block-impl")
        ("b1i" . "block1-impl")
        ("bp" . "block-parameter")
        ("ifb" . "if-block")
        ("ieb" . "ifelse-block")
        ("fore" . "for-enum")
        ("fb" . "func-brace")
        ("sb" . "sync-block")
        ("pm" . "pragma-mark")
        ("ass" . "assign-statement")
        ("sng" . "singleton-def")
        ("marray" . "[NSMutableArray array]")
        )))))

(add-hook
 'swift-mode-hook
 (lambda ()
   (mapc
    (lambda (pair)
      (define-abbrev swift-mode-abbrev-table (car pair) (cdr pair)))
    '(("pi" . "π")
      ("sng" . "singleton-def")
      ))))

(setq save-abbrevs nil)

(provide 'abbrev-config)
;;; abbrev-config.el ends here
