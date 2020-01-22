;;; package --- wrapper.el
;;; Commentary:
;;; Code:

(defun mk-import-at-point ()
  "."
  (interactive)
  (mk-wrapper "#import \"%s.h\""))

(defun mk-defun-at-point ()
  "."
  (interactive)
  (mk-wrapper "- (void)%s {\n    \n}"))

(defun mk-wrapper (format)
  "FORMAT: ."
  (interactive "sFormat: ")
  (wrapping
   '(lambda (origin-content)
      (format format origin-content))))

(defun add-pair-around-region (pair-left pair-right)
  "PAIR-LEFT: , PAIR-RIGHT: ."
  (interactive "spair-left: \nspair-right: ")
  (if (region-active-p)
      (let ((b (region-beginning))
            (e (region-end)))
        (deactivate-mark)
        (goto-char e)
        (insert pair-right)
        (goto-char b)
        (insert pair-left))))

(defun prepend-word (prefix)
  "PREFIX: ."
  (interactive "sGive me the prefix: ")
  (save-excursion
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (if bounds
          (progn
            (goto-char (car bounds))
            (insert prefix))))))

(defun wrap-paren ()
  "."
  (interactive)
  (add-pair-around-region "(" ")"))

(defun wrap-pair (one-pair)
  "ONE-PAIR: ."
  (interactive "sPlease input one pair: ")
  (cond ((or (string-equal one-pair "(")
             (string-equal one-pair ")"))
         (wrap-paren))
        ((or (string-equal one-pair "[")
             (string-equal one-pair "]"))
         (add-pair-around-region "[" "]"))
        ((or (string-equal one-pair "{")
             (string-equal one-pair "}"))
         (add-pair-around-region "{" "}"))
        ((or (string-equal one-pair "<")
             (string-equal one-pair ">"))
         (add-pair-around-region "<" ">"))
        ((string-equal one-pair "`")
         (add-pair-around-region "`" "`"))
        ((string-equal one-pair "'")
         (add-pair-around-region "'" "'"))
        ((string-equal one-pair "\"")
         (add-pair-around-region "\"" "\""))))

(defun wrap-p-pair ()
  "."
  (interactive)
  (add-pair-around-region "<p>" "</p>"))

(defun wrap-red-span ()
  "."
  (interactive)
  (add-pair-around-region "<span style=\"color: red\">" "</span>"))

(defun wrap-lisp-not ()
  "."
  (interactive)
  (add-pair-around-region "(not " ")"))


(provide 'wrapper)
;;; wrapper.el ends here