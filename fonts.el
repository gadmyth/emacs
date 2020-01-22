;;; package --- fonts.el
;;; Commentary:
;;; Code:

(defvar *use-ubuntu-font* nil)

(defmacro when-font-exist (font-name &rest body)
  "FONT-NAME: , BODY: ."
  `(let ((font-name ,font-name))
     (when (and (boundp 'x-list-fonts)
                (not (null (x-list-fonts font-name)))
                ,@body
                (message "when-font-exist: %s" font-name)
                ))))

(when-font-exist "Ubuntu Mono" (when *use-ubuntu-font*
                                 (custom-set-faces `(default ((t (:family ,font-name)))))
                                 (set-face-attribute 'default nil :height 140)))


(provide 'fonts)
;;; fonts.el ends here