;;; package --- mybatis.el
;;; Commentary:
;;; Code:


(require 's)
(require 'textmate)
(require 'yasnippet)


(defun mybatis-parse-result-line (prop)
  "Parse mybatis result line of PROP property."
  (let ((regexp (format "^.*?<result\s*.*%s=\"\\(.*?\\)\"" prop))
        (line (buffer-substring (line-beginning-position) (line-end-position))))
    (if (string-match regexp line)
        (let ((str (substring-no-properties line (match-beginning 1) (match-end 1))))
          ;; (message "parsed %s is %s" prop str)
          str))))

(defun mybatis-parse-result-property-string ()
  "."
  (interactive)
  (mybatis-parse-result-line "property"))

(defun mybatis-parse-result-jdbc-type-string ()
  "."
  (interactive)
  (mybatis-parse-result-line "jdbcType"))

(defun mybatis-parse-result-column-string ()
  "."
  (interactive)
  (mybatis-parse-result-line "column"))

(defun mybatis-parse-result-properties ()
  "."
  (interactive)
  (when (region-active-p)
    (save-excursion
      (let ((start (region-beginning))
            (end (region-end))
            (list))
        (goto-char start)
        (while (< (point) end)
          (push (mybatis-parse-result-property-string) list)
          (forward-line 1))
        (reverse list)))))

(defun mybatis-parse-result-columns ()
  "."
  (interactive)
  (when (region-active-p)
    (save-excursion
      (let ((start (region-beginning))
            (end (region-end))
            (list))
        (goto-char start)
        (while (< (point) end)
          (push (mybatis-parse-result-column-string) list)
          (forward-line 1))
        (reverse list)))))

(defun generate-mybatis-base-column-list ()
  "."
  (interactive)
  (message "<sql id=\"Base_Column_List\">")
  (let ((column-list (s-join "," (mybatis-parse-result-columns))))
    (message column-list))
  (message "</sql>"))

(defun mybatis-generate-insert-column (column)
  "COLUMN."
  (interactive)
  (let ((property (s-lower-camel-case column)))
    (message "property: %S, column: %S" property column)
    (yas-expand-snippet-with-params "mybatis-if-column-block" property column)))

(defun mybatis-generate-insert-value (column jdbc-type)
  "COLUMN, JDBC-TYPE."
  (let* ((property (s-lower-camel-case column)))
    (yas-expand-snippet-with-params "mybatis-if-column-value-block" property jdbc-type)))

(defun mybatis-generate-update-value (column jdbc-type)
  "COLUMN, JDBC-TYPE."
  (let* ((property (s-lower-camel-case column)))
    (yas-expand-snippet-with-params "mybatis-update-column-value-block" property column jdbc-type)))

(defun mybatis-generate-insert-columns ()
  "."
  (interactive)
  (let ((list))
    (save-excursion
      (goto-char 0)
      (let ((start (progn (search-forward "<resultMap" nil t 1)
                          (point)))
            (end (progn (search-forward "</resultMap>" nil t 1)
                        (point))))
        (goto-char start)
        (while (< (point) end)
          (forward-line 1)
          (let* ((column (mybatis-parse-result-column-string)))
            (message "insert column: %S" column)
            (when column
              (push column list))))))
    ;; insert column
    (message "columns: %S" list)
    (let ((len (length list))
          (i 0))
      (dolist (column (reverse list))
        (mybatis-generate-insert-column column)
        (when (< i (- len 1))
          (newline-and-indent)
          (setq i (+ i 1)))))))

(defun mybatis-parse-result-map ()
  "."
  (save-excursion
    (goto-char 0)
    (let* ((str "<resultMap")
           (start (progn (search-forward str nil t 1)
                         (point)))
           (end (progn (search-forward "</resultMap>" nil t 1)
                       (point)))
           list
           found)
      (goto-char (- start (length str)))
      (while (and (not found)
                  (< (point) end))
        (let ((regexp "^.*?<resultMap\s*id=\"\\(.*?\\)\"\s*type=\"\\(.*?\\)\"")
              (line (buffer-substring (line-beginning-position) (line-end-position))))
          (if (string-match regexp line)
              (let ((map-id (substring-no-properties line (match-beginning 1) (match-end 1)))
                    (map-type (substring-no-properties line (match-beginning 2) (match-end 2))))
                ;; (message "parsed resultMap: map-id: %s, map-type: %s" map-id map-type)
                (setq found `(,map-id ,map-type)))
            (forward-line))))
      found)))

(defun mybatis-generate-insert-values ()
  "."
  (interactive)
  (let ((list)
        (type-list))
    (save-excursion
      (goto-char 0)
      (let ((start (progn (search-forward "<resultMap" nil t 1)
                          (point)))
            (end (progn (search-forward "</resultMap>" nil t 1)
                        (point))))
        (goto-char start)
        (while (< (point) end)
          (forward-line 1)
          (let* ((column (mybatis-parse-result-column-string))
                 (jdbc-type (mybatis-parse-result-jdbc-type-string)))
            (when (and column jdbc-type)
              (push (cons column jdbc-type) list))))))
    ;; insert column and type
    (let ((len (length list))
          (i 0))
      (dolist (pair (reverse list))
        (mybatis-generate-insert-value (car pair) (cdr pair))
        (when (< i (- len 1))
          (newline-and-indent)
          (setq i (+ i 1)))))))

(defun mybatis-generate-update-values ()
  "."
  (interactive)
  (let ((list)
        (type-list))
    (save-excursion
      (goto-char 0)
      (let ((start (progn (search-forward "<resultMap" nil t 1)
                          (point)))
            (end (progn (search-forward "</resultMap>" nil t 1)
                        (point))))
        (goto-char start)
        (while (< (point) end)
          (forward-line 1)
          (let* ((column (mybatis-parse-result-column-string))
                 (jdbc-type (mybatis-parse-result-jdbc-type-string)))
            (when (and column jdbc-type)
              (push (cons column jdbc-type) list))))))
    ;; insert column and type
    (let ((len (length list))
          (i 0))
      (dolist (pair (reverse list))
        (mybatis-generate-update-value (car pair) (cdr pair))
        (when (< i (- len 1))
          (newline-and-indent)
          (setq i (+ i 1)))))))

(defun generate-sql-column-definitions ()
    "."
    (interactive)
    (when (region-active-p)
      (save-excursion
        (let ((start (region-beginning))
              (end (region-end))
              (list))
          (goto-char start)
          (while (< (point) end)
            (let* ((column (mybatis-parse-result-column-string))
                   (jdbc-type (mybatis-parse-result-jdbc-type-string)))
              (when (and column jdbc-type)
                (push (generate-sql-column-definition column jdbc-type) list))
              (forward-line 1))
            (reverse list))))))

(defun generate-sql-column-definition (column jdbc-type)
  "."
  (let ((str (format "%s\t%s,"
                     column
                     jdbc-type)))
    (message str)))

(defun mybatis-generate-insert-block ()
  "."
  (interactive)
  (let ((table-name (read-string "Please input table name: "))
        (parameter-type (cadr (mybatis-parse-result-map))))
    (yas-expand-snippet-with-params "mybatis-insert-block" parameter-type table-name)
    (yas-expand-snippet-with-params "mybatis-trim-block")
    (mybatis-generate-insert-columns)
    (forward-line)
    (end-of-line)
    (newline-and-indent)
    (yas-expand-snippet-with-params "mybatis-values-trim-block")
    (mybatis-generate-insert-values)))

(defun mybatis-generate-update-block ()
  "."
  (interactive)
  (let ((table-name (read-string "Please input table name: "))
        (parameter-type (cadr (mybatis-parse-result-map))))
    (yas-expand-snippet-with-params "mybatis-update-block" parameter-type table-name)
    (mybatis-generate-update-values)))

(defun generate-create-table-sql ()
  "."
  (interactive)
  (let ((table-name (read-string "Please input table name: ")))
    (message "create table %s" table-name)
    (message "(")
    (generate-sql-column-definitions)
    (message ")")))

(defun create-java-property-from-mybatis-column ()
  "."
  (interactive)
  (let* ((prop (mybatis-parse-column-property-string))
         (java-property (format "private String %s;" prop)))
    (kill-new java-property)))

(defun parse-properties-from-java-class ()
  "."
  (interactive)
  (let* ((raw-prop-list (scj-collect-with-regexp +java-property-regexp+ nil "No property here.")))
    (mapcar
     (lambda (raw-prop)
       ;; (message "raw-prop: %s" raw-prop)
       (when (string-match +java-property-regexp+ raw-prop)
         (let (property column type jdbc-type)
           (setq type (match-string 1 raw-prop))
           (setq property (match-string 2 raw-prop))
           (setq jdbc-type (pcase type
                             ("String" "VARCHAR")
                             ("Integer" "INTEGER")
                             ("Date" "TIMESTAMP")
                             ("int" "INTEGER")
                             ("long" "BIGINT")
                             ("Long" "BIGINT")
                             (_ (error "Can't find jdbcType for java type %s" type))))
           (setq column (s-snake-case property))
           `((property . ,property)
             (column . ,column)
             (jdbc-type . ,jdbc-type)))))
     raw-prop-list)))

(defun parse-full-java-class-name ()
  "."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^package \\(.*\\);" nil t 1)
    (when-let ((package (match-string-no-properties 1)))
      (message "package is %s" package)
      (re-search-forward "^public \\(class\\|interface\\) \\(.*\\) {" nil t 1)
      (when-let ((class-name (match-string-no-properties 2)))
        (message "class-name is %s" class-name)
        (let ((full-class-name (format "%s.%s" package class-name)))
          (message "full-class-name: %S" full-class-name)
          full-class-name)))))

(defun mybatis-create-result-map-from-java-class ()
  "."
  (interactive)
  (when-let ((project-root (textmate-find-project-root)))
    (let* ((default-directory project-root)
           (class-name (read-string "Please input the java class name: "))
           (cmd (format "cd %s; find . -name %s.java" default-directory class-name))
           (file-names (string-split (shell-command-to-string cmd)))
           (file-name (concat default-directory
                              "/"
                              (completing-read "Please select a file: " file-names nil t)))
           (full-class-name (with-temp-buffer
                              (insert-file-contents file-name)
                              (parse-full-java-class-name))))
      (yas-expand-snippet-with-params "mybatis-mapper-result-map" full-class-name)
      (let* ((prop-pairs (with-temp-buffer
                           (insert-file-contents file-name)
                           (parse-properties-from-java-class)))
             (len (length prop-pairs))
             (i 0))
        (seq-doseq (prop-pair prop-pairs)
          (yas-expand-snippet-with-params
           "mybatis-result-column"
           (assoc-default 'column prop-pair)
           (assoc-default 'jdbc-type prop-pair)
           (assoc-default 'property prop-pair))
          (when (< i (- len 1))
            (newline-and-indent)
            (setq i (+ i 1))))))))

(defun mybatis-create-mapper-java-file ()
  "."
  (interactive)
  (when-let ((project-root (textmate-find-project-root)))
    (let* ((default-directory project-root)
           (cmd (format "cd %s; find . -name mapper | grep -v target" default-directory))
           (mapper-xml-directories (shell-command-to-string cmd))
           (directories (string-split mapper-xml-directories)))
      (message "directories: %S" (type-of directories))
      (when (> (length directories) 0)
        (let* ((directory (completing-read "Please select a directory: " directories nil t))
               (file-name (read-string "Please input the mapper java file name: "))
               (full-file-name (format "%s/%s/%s.java" default-directory directory file-name))
               package-head)
          (let ((cmd (format "cd %s; cd %s; grep -E 'package .*;' $(ls | head -n 1)" default-directory directory)))
            (setq package-head (shell-command-to-string cmd)))
          (when (and
                 (> (length full-file-name) 0)
                 (not (file-exists-p full-file-name)))
            (make-empty-file full-file-name)
            (with-temp-file full-file-name
              (with-current-buffer (current-buffer)
                (java-mode)
                (insert package-head)
                (insert "\n")
                (insert (format "public interface %s {\n\    \n}\n" (file-name-sans-extension file-name)))))
            (find-file full-file-name)))))))

(defun mybatis-create-mapper-xml-file ()
  "."
  (interactive)
  (when-let ((project-root (textmate-find-project-root)))
    (let* ((default-directory project-root)
           (cmd (format "cd %s; find . -name mappers | grep -v target" default-directory))
           (mapper-xml-directories (shell-command-to-string cmd))
           (directories (string-split mapper-xml-directories)))
      (message "directories: %S" (type-of directories))
      (when (> (length directories) 0)
        (let* ((directory (completing-read "Please select a directory: " directories nil t))
               (file-name (read-string "Please input the mapper xml file name: "))
               (full-file-name (format "%s/%s/%s.xml" default-directory directory file-name))
               (ext-full-file-name (format "%s/%s/%sExt.xml" default-directory directory file-name))
               (mapper-java-class ""))
          ;; parse mapper-java-class
          (let* ((mapper-java-class-name (read-string "Please input mapper java class name: "))
                 (cmd (format "cd %s; find . -name %s.java -exec realpath {} +" default-directory mapper-java-class-name))
                 (mapper-java-class-full-name (replace-regexp-in-string "\n" "" (shell-command-to-string cmd))))
            (message "mapper-java-class-full-name: %s" mapper-java-class-full-name)
            (message "exists: %S" (file-exists-p mapper-java-class-full-name))
            (when (and
                   (> (length mapper-java-class-full-name) 0)
                   (file-exists-p mapper-java-class-full-name))
              (with-temp-buffer
                (insert-file-contents mapper-java-class-full-name)
                (let ((class-name (parse-full-java-class-name)))
                  (message "current-buffer: %s, class-name: %s" (current-buffer) class-name)
                  (setq mapper-java-class class-name)))))
          ;; create file and input template content
          (when (and
                 (> (length ext-full-file-name) 0)
                 (not (file-exists-p ext-full-file-name))
                 (y-or-n-p "Create extension mapper xml file? "))
            (make-empty-file ext-full-file-name)
            (with-temp-file ext-full-file-name
              (with-current-buffer (current-buffer)
                (nxml-mode)
                (yas-minor-mode)
                (message "template param: %s" mapper-java-class)
                (yas-expand-snippet-with-params "mybatis-xml-file" mapper-java-class))))
          (when (and
                 (> (length full-file-name) 0)
                 (not (file-exists-p full-file-name)))
            (make-empty-file full-file-name)
            (with-temp-file full-file-name
              (with-current-buffer (current-buffer)
                (nxml-mode)
                (yas-minor-mode)
                (message "template param: %s" mapper-java-class)
                (yas-expand-snippet-with-params "mybatis-xml-file" mapper-java-class)
                (mybatis-create-result-map-from-java-class)
                (re-search-forward "</resultMap>")
                (newline-and-indent 2)
                (mybatis-generate-insert-block)
                (re-search-forward "</insert>")
                (newline-and-indent 2)
                (mybatis-generate-update-block)))
            (find-file full-file-name)))))))

(provide 'mybatis)
;;; mybatis.el ends here
