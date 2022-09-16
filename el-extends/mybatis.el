;;; package --- mybatis.el
;;; Commentary:
;;; Code:


(require 's)


(defun parse-mybatis-result-line (prop)
  "Parse mybatis result line of PROP property."
  (let ((regexp (format "^.*?<result\s*.*%s=\"\\(.*?\\)\"" prop))
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
  (message "<sql id=\"Base_Column_List\">")
  (let ((column-list (s-join "," (parse-mybatis-result-columns))))
    (message column-list))
  (message "</sql>"))

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
            (when column
              (push (generate-mybatis-insert-column column) list))
            (forward-line 1))
          (reverse list))))))

(defun parse-mybatis-result-map ()
  "."
  (when (region-active-p)
    (save-excursion
      (let ((start (region-beginning))
            (end (region-end))
            list
            found)
        (goto-char start)
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
        found))))

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
            (when (and column jdbc-type)
              (push (generate-mybatis-insert-value column jdbc-type) list))
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
            (when (and column jdbc-type)
              (push (generate-mybatis-update-value column jdbc-type) list))
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

(defun generate-mybatis-insert-block ()
  "."
  (interactive)
  (let ((table-name (read-string "Please input table name: ")))
    (message "<insert id=\"insert\" parameterType=\"%s\">" (cadr (parse-mybatis-result-map)))
    (message "insert into %s" table-name)
    (message "<trim prefix=\"(\" suffix=\")\" suffixOverrides=\",\">")
    (generate-mybatis-insert-columns)
    (message "</trim>")
    (message "<trim prefix=\"values (\" suffix=\")\" suffixOverrides=\",\">")
    (generate-mybatis-insert-values)
    (message "</trim>")
    (message "</insert>")))

(defun generate-mybatis-update-block ()
  "."
  (interactive)
  (let ((table-name (read-string "Please input table name: ")))
    (message "<update id=\"update\" parameterType=\"%s\">" (cadr (parse-mybatis-result-map)))
    (message "update %s" table-name)
    (message "<set>")
    (generate-mybatis-update-values)
    (message "</set>")
    (message "where id = #{id,jdbcType=BIGINT}")
    (message "</update>")))

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
                             ("Date" "VARCHAR")
                             ("int" "INTEGER")
                             ("long" "INTEGER")
                             (_ (error "Can't find jdbcType for java type %s" type))))
           (setq column (s-snake-case property))
           `((property . ,property)
             (column . ,column)
             (jdbc-type . ,jdbc-type)))))
     raw-prop-list)))

(defun generate-mybatis-result-map-from-java-class ()
  "."
  (interactive)
  (message "<resultMap id=\"BaseResultMap\" type=\"class\">")
  (seq-doseq (prop-pair (parse-properties-from-java-class))
    (message "<result column=\"%s\" jdbcType=\"%s\" property=\"%s\"/>"
             (assoc-default 'column prop-pair)
             (assoc-default 'jdbc-type prop-pair)
             (assoc-default 'property prop-pair)))
  (message "</resultMap>"))

(provide 'mybatis)
;;; mybatis.el ends here
