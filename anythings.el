;;; package --- anythings.el
;;; Commentary:
;;; Code:


(require-packages-if-installed
 '(anything 'anything-config)
 (defvar anything-c-source-objc-headline
   '((name . "Objective-C Headline")
     (headline  "^[-+@]\\|^#pragma mark")))

 (defun objc-headline ()
   (interactive)
   ;; Set to 500 so it is displayed even if all methods are not narrowed down.
   (let ((anything-candidate-number-limit 500))
     (anything-other-buffer '(anything-c-source-objc-headline)
			    "*ObjC Headline*"))))

(provide 'anythings)
;;; anythings.el ends here
