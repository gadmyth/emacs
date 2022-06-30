;;; package --- mybatis.el
;;; Commentary:
;;; Code:


(require 's)


(defun parse-mybatis-result-line (prop)
  "Parse mybatis result line of PROP property."
  (let ((regexp (format "^.*?\s*%s=\"\\(.*?\\)\"" prop))
        (line (buffer-substring (line-beginning-position) (line-end-position))))
    (if (string-match regexp line)
        (let ((str (substring-no-properties line (match-beginning 1) (match-end 1))))
          ;; (message "parsed %s is %s" prop str)
          str))))

(defun parse-mybatis-result-property-string ()
  "."
  (interactive)
  (parse-mybatis-result-line "property"))

(defun parse-mybatis-result-jdbc-type-string ()
  "."
  (interactive)
  (parse-mybatis-result-line "jdbcType"))

(defun parse-mybatis-result-column-string ()
  "."
  (interactive)
  (parse-mybatis-result-line "column"))

(defun parse-mybatis-result-properties ()
  "."
  (interactive)
  (when (region-active-p)
    (save-excursion
      (let ((start (region-beginning))
            (end (region-end))
            (list))
        (goto-char start)
        (while (< (point) end)
          (push (parse-mybatis-result-property-string) list)
          (forward-line 1))
        (reverse list)))))

(defun parse-mybatis-result-columns ()
  "."
  (interactive)
  (when (region-active-p)
    (save-excursion
      (let ((start (region-beginning))
            (end (region-end))
            (list))
        (goto-char start)
        (while (< (point) end)
          (push (parse-mybatis-result-column-string) list)
          (forward-line 1))
        (reverse list)))))

(defun generate-mybatis-base-column-list ()
  "."
  (interactive)
  (let ((column-list (s-join "," (parse-mybatis-result-columns))))
    (message column-list)))

(defun generate-mybatis-insert-column (column)
  "."
  (let* ((property (s-lower-camel-case column))
         (str (format "<if test=\"%s != null\">\n\t%s,\n</if>"
                      property
                      column)))
    (message str)))

(defun generate-mybatis-insert-value (column jdbc-type)
  "."
  (let* ((property (s-lower-camel-case column))
         (str (format "<if test=\"%s != null\">\n\t#{%s,jdbcType=%s},\n</if>"
                     property
                     property
                     jdbc-type)))
    (message str)))

(defun generate-mybatis-update-value (column jdbc-type)
  "."
  (let* ((property (s-lower-camel-case column))
         (str (format "<if test=\"%s != null\">\n\t%s = #{%s,jdbcType=%s},\n</if>"
                      property
                      column
                      property
                      jdbc-type)))
    (message str)))

(defun generate-mybatis-insert-columns ()
  "."
  (interactive)
  (when (region-active-p)
    (save-excursion
      (let ((start (region-beginning))
            (end (region-end))
            (list))
        (goto-char start)
        (while (< (point) end)
          (let* ((column (parse-mybatis-result-column-string)))
            (push (generate-mybatis-insert-column column) list)
            (forward-line 1))
          (reverse list))))))

(defun generate-mybatis-insert-values ()
  "."
  (interactive)
  (when (region-active-p)
    (save-excursion
      (let ((start (region-beginning))
            (end (region-end))
            (list))
        (goto-char start)
        (while (< (point) end)
          (let* ((column (parse-mybatis-result-column-string))
                 (jdbc-type (parse-mybatis-result-jdbc-type-string)))
            (push (generate-mybatis-insert-value column jdbc-type) list)
            (forward-line 1))
          (reverse list))))))

(defun generate-mybatis-update-values ()
  "."
  (interactive)
  (when (region-active-p)
    (save-excursion
      (let ((start (region-beginning))
            (end (region-end))
            (list))
        (goto-char start)
        (while (< (point) end)
          (let* ((column (parse-mybatis-result-column-string))
                 (jdbc-type (parse-mybatis-result-jdbc-type-string)))
            (push (generate-mybatis-update-value column jdbc-type) list)
            (forward-line 1))
          (reverse list))))))

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
          (let* ((column (parse-mybatis-result-column-string))
                 (jdbc-type (parse-mybatis-result-jdbc-type-string)))
            (push (generate-sql-column-definition column jdbc-type) list)
            (forward-line 1))
          (reverse list))))))

(defun generate-sql-column-definition (column jdbc-type)
  "."
  (let ((str (format "%s\t%s,"
                     column
                     jdbc-type)))
    (message str)))

(defun generate-mybatis-insert-block ()
  "."
  (interactive)
  (let ((table-name (read-string "Please input table name: ")))
    (message "insert into %s" table-name)
    (message "<trim prefix=\"(\" suffix=\")\" suffixOverrides=\",\">")
    (generate-mybatis-insert-columns)
    (message "</trim>")
    (message "<trim prefix=\"values (\" suffix=\")\" suffixOverrides=\",\">")
    (generate-mybatis-insert-values)
    (message "</trim>")))

(defun generate-mybatis-update-block ()
  "."
  (interactive)
  (let ((table-name (read-string "Please input table name: ")))
    (message "update %s" table-name)
    (message "<set>")
    (generate-mybatis-update-values)
    (message "</set>")
    (message "where id = #{id,jdbcType=BIGINT}")))

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
  (let* ((prop (parse-mybatis-column-property-string))
         (java-property (format "private String %s;" prop)))
    (kill-new java-property)))

(provide 'mybatis)
;;; mybatis.el ends here
