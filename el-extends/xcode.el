;;; package --- xcode.el
;;; Commentary:
;;; Code:

(require 'ivy)
(require 'source-jump)

(defun read-xcasset-directory (root-directory imageset-handler)
  "ROOT-DIRECTORY is the root directory of the Assets.xcassets dir, IMAGESET-HANDLER is a lambda or function with a imageset dir parameter."
  (let ((dir (expand-file-name root-directory)))
    (ivy-read "Choose imageset: "
              (split-string (shell-command-to-string (format "find %s -name *.imageset" dir))) :action imageset-handler)))


;;; You should config the *xcode-project-directory* in extra config el file.
(defvar *xcode-project-directory* '())

(defun copy-xcasset-directory (handler)
  "HANDLER: ."
  (ivy-read "Choose project directory: " *xcode-project-directory*
            :action (lambda (project-directory)
                      (read-xcasset-directory project-directory
                                              (lambda (imageset)
                                                (funcall handler imageset))))))

(defun dired-copy-file-to-imageset ()
  "."
  (interactive)
  (when (string-equal major-mode "dired-mode")
    (setq-local current-line-file nil)
    (setq-local current-line-file (dired-file-name-at-point))
    (when current-line-file
      (copy-xcasset-directory (lambda (imageset)
                                (if (or (string-suffix-p "@2x.png" current-line-file)
                                        (string-suffix-p "@3x.png" current-line-file))
                                    (when-let ((suffix (cond ((string-suffix-p "@2x.png" current-line-file) "@2x.png")
                                                             ((string-suffix-p "@3x.png" current-line-file) "@3x.png")
                                                             (t nil))))
                                      (let ((to-file (or (car (split-string (shell-command-to-string (format "find %s -name \"*%s\"" imageset suffix))))
                                                         (format "%s/%s%s" imageset (file-name-base imageset) suffix))))
                                        (message "from: %s\nto: %s" current-line-file to-file)
                                        (copy-file current-line-file to-file t)))))))))


(defun goto-buffer (buffername)
  "BUFFERNAME: ."
  (if-let ((buffer (get-buffer buffername)))
      (switch-to-buffer buffer)
    (progn
      (counsel-git buffername))))

(defun objc-goto-import ()
  "."
  (interactive)
  (sj-goto-with-regexp "^#import .*$" "The import: " "No import here."))

(defun objc-goto-last-import ()
  "."
  (interactive)
  (sj-goto-last-with-regexp "^#import .*$" "The import: " "No import here."))

(defun insert-import (header-string)
  "HEADER-STRING."
  (interactive "sHeader File: ")
  (objc-goto-last-import)
  (move-end-of-line 1)
  (insert "\n")
  (insert (format "#import \"%s.h\"" header-string)))

(defun objc-goto-method ()
  "."
  (interactive)
  (sj-goto-with-regexp "^- (.*).*$" "The method: " "No methods here."))

(defun objc-goto-class ()
  "."
  (interactive)
  (sj-goto-with-regexp "^@interface.*$" "The interface: " "No interface here."))

(defun objc-goto-implementation ()
  "."
  (interactive)
  (sj-goto-with-regexp "^@implementation.*$" "The implementation: " "No implementation here."))

(defun objc-goto-property ()
  "."
  (interactive)
  (sj-goto-with-regexp "^@property.*$" "The properties: " "No porperties here."))

(defun objc-goto-last-property ()
  "."
  (interactive)
  (sj-goto-last-with-regexp "^@property.*$" "The properties: " "No porperties here."))

(defun objc-prepare-insert-property ()
  "."
  (interactive)
  (objc-goto-last-property)
  (move-end-of-line 1)
  (insert "\n"))

(defun objc-insert-property (type prop-name)
  "TYPE, PROP-NAME."
  (interactive "sType: \nsProperty name: ")
  (objc-prepare-insert-property)
  (yas-expand-snippet-with-params "prop-def" "__default__" type prop-name))

(defun objc-insert-property-2 (type prop-name)
  "SNIPPET-NAME, TYPE, PROP-NAME."
  (interactive "sType: \nsProperty name: ")
  (objc-prepare-insert-property)
  (yas-expand-snippet-with-params "prop2-def" type prop-name))

(provide 'xcode)
;;; xcode.el ends here