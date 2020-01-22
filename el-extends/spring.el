;;; package --- spring.el
;;; Commentary:
;;; Code:

(require 'counsel)
(require 'source-jump)
(require 'cl)

(defun java-goto-class (class &optional finish-block noselect)
  "CLASS, FINISH-BLOCK, NOSELECT."
  (interactive "sClass: ")
  (let* ((default-directory (expand-file-name (counsel-locate-git-root)))
         (extension "java")
         (full-cmd (format "git ls-files | egrep \"%s(Impl)?\\>.*?.%s\"" class extension))
         (cands (split-string (shell-command-to-string full-cmd) "\n" t)))
    (flet ((candidate-action (filename)
                             (let ((buffer (if noselect (find-file-noselect filename) (find-file filename))))
                               (if finish-block (funcall finish-block buffer)))))
      (if (equal 1 (length cands))
          (candidate-action (car cands))
        (ivy-read "Select class: " (reverse cands) :action
                  (lambda (candidate)
                    (candidate-action candidate)))))))

(defun java-goto-class-at-point ()
  "."
  (interactive)
  (let ((class (word-at-point)))
    (java-goto-class class)))

(defun spring-goto-interface ()
  "."
  (interactive)
  (save-excursion
    (java-goto-class-def)
    (when (re-search-forward "\\<implements\\>" nil t)
      (forward-word)
      (java-goto-class-at-point))))

(defun java-goto-class-def ()
  "."
  (interactive)
  (let* ((class-name (file-name-base (buffer-name)))
         (regexp (format "^public class\\|interface %s.*$" class-name)))
    (sj-action-with-regexp regexp nil "No class here."
                           (apply-partially #'sj-goto-line-or-select "Line content: "))))

(defconst +java-method-format+ "^.*? \\(public\\|private\\) .*\\(%s\\)(.*)[^()]*{\s*$")

(defun java-goto-method (&optional method)
  "METHOD."
  (interactive)
  (let ((regexp (format +java-method-format+ (or method ".*"))))
    (message "java-goto-method: %s" regexp)
    (sj-action-with-regexp regexp nil "No methods here."
                           (apply-partially #'sj-goto-line-or-select "The method: "))))

(defun java-goto-method-at-point ()
  "."
  (interactive)
  (let ((method (word-at-point)))
    (java-goto-method method)))

(defun java-goto-last-method ()
  "."
  (interactive)
  (let ((regexp (format +java-method-format+ ".*")))
    (sj-goto-last-with-regexp regexp  "The method name: " "No method here.")))

(defun java-jump-to-definition (variable)
  "VARIABLE."
  (let ((regexp (format "\\<\\([A-Z].*\\)\\>\s*%s\s*?[;),=]" variable))
        class)
    (re-search-backward regexp nil t)
    (setq class (match-string-no-properties 1))
    (message "class: %S" class)
    class))

(defun java-jump-to-definition-at-point ()
  "."
  (interactive)
  (let ((variable (word-at-point)))
    (message variable)
    (java-jump-to-definition variable)))

(defun java-resolve-class-method (&optional action)
  "ACTION."
  (interactive)
  (let ((method (word-at-point))
        (current-point (point)))
    (strip-text-properties method)
    (sj-save-excursion
     (re-search-backward "\\." nil t)
     (backward-word)
     (let* ((class (word-at-point))
            (first-char (elt class 0)))
       (message "method: %s, class: %s" method class)
       (if (not (and (<= ?A first-char) (<= first-char ?Z)))
           (setq class (java-jump-to-definition class)))
       (goto-char current-point)
       (cons class method)))))

(defun java-jump-to-class-method ()
  "."
  (interactive)
  (let* ((pair (java-resolve-class-method))
         (class (car pair))
         (method (cdr pair)))
    (message "java-jump-to-class-method: %S, %S" class method)
    (java-goto-class class
                     #'(lambda (buffer) (java-goto-method method)))))

(defun java-list-class-method (&optional action)
  "ACTION."
  (interactive)
  (let* ((current-buffer (current-buffer))
         (pair (java-resolve-class-method))
         (class (car pair))
         (method (cdr pair)))
    (java-goto-class class
                     #'(lambda (buffer)
                         (let ((regexp (format +java-method-format+ "[^\s]*")))
                           (message "regexp: %s, class: %s, buffer: %S" regexp class buffer)
                           (sj-action-with-regexp regexp nil "No methods here."
                                                  (lambda-of-ivy-read
                                                   "The methods: "
                                                   (let* ((candidate-string (car candidate))
                                                          (candidate-string (substring candidate-string)))
                                                     (if (string-match regexp candidate-string)
                                                         (let ((matched-method (substring candidate-string (match-beginning 1) (match-end 1))))
                                                           (message "method: %s" matched-method)
                                                           (with-current-buffer current-buffer
                                                             (if action (funcall action matched-method)))))))
                                                  buffer)))
                     t)))

(defun java-insert-class-method ()
  "."
  (interactive)
  (java-list-class-method #'(lambda (method)
                              (insert method))))

(defconst +java-constant-regexp-format+ "\\(private\\|public\\).*%s.*$\\|^\s*%s(.*).*$")
(defun java-goto-constant (constant)
  "CONSTANT."
  (interactive "sConstant: ")
  (let ((regexp (format +java-constant-regexp-format+ constant constant)))
    (sj-goto-last-with-regexp regexp "The constant's name: " "No constant here.")))

(defun java-goto-constant-at-point ()
  "."
  (interactive)
  (let* ((word (word-at-point)))
    (java-goto-constant word)))

(defun java-show-constant (constant)
  "CONSTANT."
  (interactive "sConstant: ")
  (let ((regexp (format +java-constant-regexp-format+ constant constant))
        (current-point (point)))
    (sj-show-last-with-regexp regexp "The constant's name: " "No constant here."
                              #'(lambda ()
                                  (goto-char current-point)))))

(defun java-show-constant-at-point ()
  "."
  (interactive)
  (let* ((word (word-at-point)))
    (java-show-constant word)))

(defun java-jump-to-class-constant ()
  "."
  (interactive)
  (let ((constant (word-at-point))
        (current-point (point)))
    (sj-save-excursion
     (re-search-backward "\\." nil t)
     (backward-word)
     (let* ((class (word-at-point))
            (first-char (elt class 0)))
       (goto-char current-point)
       (message "constant: %s, class: %s" constant class)
       (if (not (and (<= ?A first-char) (<= first-char ?Z)))
           (setq class (java-jump-to-definition class)))
       (if class
           (java-goto-class class
                            #'(lambda (buffer) (java-goto-constant constant))))))))

(defun java-show-class-constant ()
  "."
  (interactive)
  (let ((constant (word-at-point))
        (current-point (point)))
    (sj-save-excursion
     (re-search-backward "\\." nil t)
     (backward-word)
     (let* ((class (word-at-point))
            (first-char (elt class 0)))
       (goto-char current-point)
       (message "constant: %s, class: %s" constant class)
       (if (not (and (<= ?A first-char) (<= first-char ?Z)))
           (setq class (java-jump-to-definition class)))
       (if class
           (java-goto-class class
                            #'(lambda (buffer)
                                (with-current-buffer buffer
                                  (java-show-constant constant)))
                            t))))))

(defconst +java-property-regexp+ "^\s*?private\s*.*;\s*?$")

(defun java-goto-property ()
  "."
  (interactive)
  (sj-goto-with-regexp +java-property-regexp+ "The property name: " "No property here."))

(defun java-goto-last-property ()
  "."
  (interactive)
  (sj-goto-last-with-regexp +java-property-regexp+ "The property name: " "No property here."))

(defun java-create-property (prop-name type)
  "PROP-NAME, TYPE."
  (interactive "sProperty Name: \nsType: ")
  (java-goto-class-def)
  (java-goto-last-property)
  (move-end-of-line 1)
  (insert "\n\n")
  (insert (format "private %s %s;" type prop-name))
  (indent-region (point) (point)))

(defun java-create-setter-getter (prop-name type)
  "PROP-NAME, TYPE."
  (interactive "sProperty Name: \nsType: ")
  (java-goto-class-def)
  (when (re-search-forward "{" nil t 1)
    (evil-jump-item)
    (previous-line)
    (move-end-of-line 1)
    (insert "\n\n")
    (let ((current-point (point)))
      (yas-expand-snippet-with-params "prop-setter" prop-name type)
      (insert "\n\n")
      (yas-expand-snippet-with-params "prop-getter" prop-name type)
      (indent-region current-point (point)))))

(defun java-create-property-suite (prop-name type)
  "PROP-NAME, TYPE."
  (interactive "sProperty Name: \nsType: ")
  (java-create-property prop-name type)
  (java-create-setter-getter prop-name type))

(defun spring-goto-mapper-xml-file (mapper &optional finish-block)
  "MAPPER, FINISH-BLOCK."
  (interactive "sMapper: \nsMethod: ")
  (let* ((default-directory (expand-file-name (counsel-locate-git-root)))
         (extension "xml")
         (full-cmd (format "git ls-files | egrep \"\\<%s\\>.%s\"" mapper extension))
         (cands (split-string (shell-command-to-string full-cmd) "\n" t)))
    (if (equal 1 (length cands))
        (progn
          (find-file (car cands))
          (if finish-block (funcall finish-block)))
      (ivy-read "Select mapper file: " (reverse cands) :action
              (lambda (candidate)
                (find-file candidate)
                (if finish-block (funcall finish-block)))))))

(defun spring-mapper-xml-goto-method (method)
  "METHOD."
  (interactive "sMethod: ")
  (let ((regexp (format "^.*? %s(.*)[^()]*{\s*$" method)))
    (sj-action-with-regexp regexp nil "No methods here."
                           (apply-partially #'sj-goto-line-or-select "The method: "))))

(defun spring-jump-to-mapper-xml-method ()
  "."
  (interactive)
  (let ((method (word-at-point))
        (current-point (point)))
    (sj-save-excursion
     (re-search-backward "\\." nil t)
     (backward-word)
     (let* ((class (word-at-point))
            (first-char (elt class 0)))
       (goto-char current-point)
       (if (not (and (<= ?A first-char) (<= first-char ?Z)))
           (setq class (java-jump-to-definition class)))
       (message "method: %s, class: %s" method class)
       (if class
           (spring-goto-mapper-xml-file class
                                        #'(lambda ()
                                            (sj-goto-last-with-regexp method "The method: " "No method here."))))))))

(defun spring-git-grep (regexp initial-word)
  "REGEXP, INITIAL-WORD."
  (interactive "sWord: ")
  (let* ((default-directory (expand-file-name (counsel-locate-git-root)))
         (full-cmd (format "git grep %s" regexp))
         (results (shell-command-to-string full-cmd))
         (cands (split-string results "\n" t)))
    (ivy-read "Select candidate: " (reverse cands)
              :initial-input
              initial-word
              :action #'(lambda (candidate)
                          (require 's)
                          (let* ((trim-candidate (s-trim candidate))
                                 (array (split-string trim-candidate ":"))
                                 (file (first array))
                                 (line-content (second array)))
                            (find-file file)
                            (goto-char (point-min))
                            (search-forward line-content))))))

(defun java-property-file-candidates ()
  "."
  (interactive)
  (goto-char (point-min))
  (let ((regexp "^[^#][^=]+=[^=]+$")
        (collections '())
        (end-of-buffer nil)
        (line-begin-pos 0)
        (line-end-pos 0)
        (line-string nil))
    (while (not end-of-buffer)
      (skip-chars-backward "^\n")
      (setq line-begin-pos (point))
      (skip-chars-forward "^\n")
      (setq line-end-pos (point))
      (setq end-of-buffer (equal line-end-pos (point-max)))
      (setq line-string (buffer-substring line-begin-pos line-end-pos))
      (setq line-string-no-properties (buffer-substring-no-properties line-begin-pos line-end-pos))
      (if (string-match regexp line-string-no-properties)
          (push (list line-string (line-number-at-pos (point))) collections))
      (if (not end-of-buffer)
          (next-line 1)))
    collections))

(defun java-property-file-show-candidates ()
  "."
  (interactive)
  (let ((regexp "^[^#][^=]+=[^=]+$"))
    (sj-action-with-regexp regexp nil "No line content here."
                           (apply-partially #'sj-goto-line-or-select "Line content: "))))


(defun git-ls-files (regexp)
  "REGEXP."
  (interactive)
  (let ((default-directory (expand-file-name (counsel-locate-git-root)))
        (command (format "git ls-files | egrep \"%s\"" regexp)))
    (split-string (shell-command-to-string command) "\n" t)))

(defun spring-list-application-properties ()
  "."
  (interactive)
  (let ((default-directory (expand-file-name (counsel-locate-git-root)))
        (cands (git-ls-files  "application.*.properties")))
    (flet ((list-properties
            (properties-file)
            (with-current-buffer (find-file-noselect properties-file)
              (let ((collections (java-property-file-candidates)))
                (ivy-read "Select property: " (reverse collections) :action nil)))))
      (if (equal 1 (length cands))
          (list-properties (car cands))
        (ivy-read "Select class: " (reverse cands) :action
                  (lambda (candidate)
                    (list-properties candidate)))))))


(defun spring-git-grep-at-point ()
  "."
  (interactive)
  (let ((word (word-at-point)))
    (spring-git-grep word word)))

(defun strip-text-properties (string)
  "STRING."
  (set-text-properties 0 (length string) nil string))

(defun spring-list-api (&optional api-action)
  "API-ACTION."
  (interactive)
  (let ((default-directory (expand-file-name (counsel-locate-git-root)))
        (candidates (git-ls-files "ApiController.java")))
    (message "api-file1: %S" candidates)
    (if-let ((api-file (car candidates)))
        (let ((buffer (find-file-noselect api-file))
              (regexp "^\s*\\(public\\|private\\) .*=.*$"))
          (message "api-file: %s, %s" api-file buffer)
          (sj-action-with-regexp regexp nil "No constant here."
                                 #'(lambda (collections)
                                     (ivy-read "The constant: " (reverse collections) :action
                                               #'(lambda (candidate)
                                                   (let* ((candidate-string (car candidate))
                                                          (candidate-string (substring candidate-string)))
                                                     (if api-action (funcall api-action candidate-string))))))
                                 buffer)))))


(defun spring-goto-api ()
  "API."
  (interactive)
  (spring-list-api #'(lambda (api-line)
                       (if (string-match "\s*\\([^\s]*\\)\s*=.*" api-line)
                           (let* ((api-constant (substring api-line (match-beginning 1) (match-end 1)))
                                  (regexp (format "\"@RequestMapping(.*\\<%s\\>.*)\"" api-constant)))
                             (message "regexp: %s" regexp)
                             (spring-git-grep regexp api-constant))))))
    
(provide 'spring)
;;; spring.el ends here