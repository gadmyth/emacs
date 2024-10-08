;;; package --- org-config.el
;;; Commentary:
;;; Code:

(require 'org)

;;; todo-keywords
(setq org-todo-keywords '((sequence "DESIGN(s)" "TODO(t)" "REDO(r)" "DUPLICATED(D)" "DOING(i)" "WARMUP(w)" "BLOCKED(b)" "CLOSING(c)" "PENDING(p)" "PLAN(l)" "|" "DONE(d!)" "ABORT(a@/!)")))

(setq org-todo-keyword-faces '(("PENDING" . (:background "LightGreen" :foreground "black" :weight bold))
                               ("PLAN" . (:background "LightGray" :foreground "white" :weight bold))
                               ("DESIGN" . (:background "LightGreen" :foreground "white" :weight bold))
                               ("DOING" . (:background "Green" :foreground "white" :weight bold))
                               ("WARMUP" . (:background "DarkSeaGreen1" :foreground "orange" :weight bold))
                               ("BLOCKED" . (:background "Purple" :foreground "white" :weight bold))
                               ("DUPLICATED" . (:background "Brown" :foreground "white" :weight bold))
                               ("CLOSING" . (:background "Orange" :foreground "white" :weight bold))
                               ("TODO" . (:background "DarkOrange" :foreground "black" :weight bold))
                               ("REDO" . (:background "DarkOrange" :foreground "black" :weight bold))
                               ("DONE" . (:background "azure" :foreground "Darkgreen" :weight bold))
                               ("ABORT" . (:background "gray" :foreground "black"))))
(setq org-log-done 'time)

;;; priority
(setq org-highest-priority ?A)
(setq org-lowest-priority ?E)
(setq org-default-priority ?E)
(setq org-priority-faces '((?A . (:background "red" :foreground "white" :weight bold))
                           (?B . (:background "DarkOrange" :foreground "white" :weight bold))
                           (?C . (:background "yellow" :foreground "DarkGreen" :weight bold))
                           (?D . (:background "DodgerBlue" :foreground "black" :weight bold))
                           (?E . (:background "SkyBlue" :foreground "black" :weight bold))
                           (?F . (:background "LightSkyBlue" :foreground "black" :weight bold))))

;;; taglist
;;(setq org-tag-alist '(("@work" . ?w) ("@home" . ?h) ("laptop" . ?l)))

;;; dependencies
(setq org-enforce-todo-dependencies t)

;;; agenda
(require 'org-agenda)

;; config files
(set-when-non-null org-agenda-diary-file *diary-file*)
(set-when-non-null diary-file *diary-file*)

(setq org-agenda-span 'day)
(setq org-agenda-include-diary t)
;; use agenda time grid
(setq org-agenda-use-time-grid t)
;; org agenda time grid
(setq org-agenda-time-grid
      `((daily today require-timed)
        ,(number-sequence 300 2400 300)
        "......" "----------------"))

;; set (longitude, latitude) for sunrise and sunset
(require 'calendar-config)

(defun diary-sunrise ()
  "."
  (setq date (calendar-current-date))
  (let* ((dss (diary-sunrise-sunset)))
    (with-temp-buffer
      (insert dss)
      (goto-char (point-min))
      (while (re-search-forward " ([^)]*)" nil t)
        (replace-match "" nil nil))
      (goto-char (point-min))
      (search-forward ",")
      (buffer-substring (point-min) (match-beginning 0)))))

(defun diary-sunset ()
  "."
  (setq date (calendar-current-date))
  (let ((dss (diary-sunrise-sunset))
        start end)
    (with-temp-buffer
      (insert dss)
      (goto-char (point-min))
      (while (re-search-forward " ([^)]*)" nil t)
        (replace-match "" nil nil))
      (goto-char (point-min))
      (search-forward ", ")
      (setq start (match-end 0))
      (search-forward " at")
      (setq end (match-beginning 0))
      (goto-char start)
      (capitalize-word 1)
      (buffer-substring start end))))

(global-set-key (kbd "C-c a") 'org-agenda)

;;; capture
(require 'org-capture)

(setq org-default-notes-file (concat org-directory "/notes.org"))
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      '(("t" "Todo" entry (file+datetree "~/org/task.org")
         "* TODO %^{Decription}\n %^U %^g\n %i")
        ("T" "Timeline" entry (file+datetree "~/org/timeline.org")
         "* %^{Decription} %^g\n %^U \n %i")
        ("e" "event" entry (file+datetree "~/org/timeline.org")
         "* %^{Decription} :event: \n %U \n %i")
        ("r" "routine" entry (file+datetree "~/org/timeline.org")
         "* %^{Decription} :routine: \n %U \n %i")
        ("p" "problem" entry (file+datetree "~/org/timeline.org")
         "* %^{Decription} :problem: \n %U \n %i")
        ("P" "plan" entry (file+datetree "~/org/timeline.org")
         "* %^{Decription} :plan: \n %U \n %i")
        ("s" "summary" entry (file+datetree "~/org/timeline.org")
         "* %^{Decription} :summary: \n %U \n %i")
        ("i" "timeline point" plain (file+datetree "~/org/timeline.org")
         "%U\n%^{Decription}\n%i")
        ("n" "Note" entry (file+headline "~/org/notes.org" "Notes")
         "* %A %^g %i")
        ("f" "Someday" entry (file+headline "~/org/task.org" "Someday")
         "* %?")
        ))

;; config for global id link
(require 'org-id)
(setq org-id-link-to-org-use-id t)

;;; org html config
;; (setq org-html-head
;;       (format "<style type='text/css'>%s</style>"
;;               (with-temp-buffer
;;                 (setq default-directory (expand-file-name "~/emacs"))
;;                 (insert-file-contents "org.css")
;;                 (buffer-string))))

;; (setq org-html-head
;;       (with-temp-buffer
;;                 (setq default-directory (expand-file-name "~/emacs"))
;;                 (insert-file-contents "theme-readtheorg.style")
;;                 (buffer-string)))

(defvar *org-export-res-files* '("htmlize.css"
                                 "readtheorg.css"
                                 "icon.css"
                                 "jquery.min.js"
                                 "bootstrap.min.js"
                                 "jquery.stickytableheaders.min.js"
                                 "readtheorg.js"
                                 "customize.js"))

(setq org-html-head-default
      (apply #'concat
             "<link rel=\"shortcut icon\" href=\"images/favicon.ico\" type=\"image/x-icon\">"
             (mapcar (lambda (file)
                       (setq default-directory (expand-file-name "~/emacs/org/res"))
                       (let* ((css-p (string-suffix-p ".css" file))
                              (css-left-pair "<style type='text/css'>")
                              (css-right-pair "</style>")
                              (js-left-pair "<script type='text/javascript'>")
                              (js-right-pair "</script>")
                              (left-pair (if css-p css-left-pair js-left-pair))
                              (right-pair (if css-p css-right-pair js-right-pair)))
                         (with-temp-buffer
                           (insert left-pair)
                           (insert-file-contents file) (goto-char (point-max))
                           (insert right-pair)
                           (buffer-string))))
                     *org-export-res-files*)))

(setq org-html-head org-html-head-default)

(defun m/org-html-checkbox (checkbox)
  "Format CHECKBOX into HTML."
  (cl-case checkbox (on "<span class=\"check\">&#x2611;</span>") ; checkbox (checked)
        (off "<span class=\"checkbox\">&#x2610;</span>")
        (trans "<span class=\"checkbox\">&#x229F;")
        (t "")))

(defadvice org-html-checkbox (around sacha activate)
  (setq ad-return-value (m/org-html-checkbox (ad-get-arg 0))))

(setq org-export-preserve-breaks "<br>")

(eval-after-load "org"
  '(progn
     (setq org-startup-indented t)
     (setq org-startup-folded "showall")
     (require-package 'valign (valign-mode))))

;;; setup org src code color
(setq org-src-fontify-natively t)

;;; src block indent
(setq org-edit-src-content-indentation 0)

;;; inline image size
(setq org-image-actual-width nil)

;;; return to open link
(setq org-return-follows-link t)

;;; superscripts
(setq-default org-use-sub-superscripts '{})
(setq-default org-export-with-sub-superscripts '{})

;;; underline
(setq-default org-export-with-emphasize nil)

;;; tag
(setq org-export-with-tags nil)


;;; babel language
(require-package 'ob-restclient)
(require-package 'ob-jq)

(org-babel-do-load-languages
 'org-babel-load-languages
 `((ditaa . t)
   (plantuml . t)
   (dot . t)
   (shell . t)
   (ruby . t)
   (python . t)
   (gnuplot . t)
   (sql . t)
   (js . t)
   (restclient . ,(featurep 'ob-restclient))
   (jq . ,(featurep 'ob-jq))
   ))

;;; headline levels
(setq org-export-headline-levels 5)
;;; toc level
(setq org-export-with-toc 5)

(setq org-ditaa-jar-path (expand-file-name "~/libs/ditaa.jar"))
(setq org-plantuml-jar-path (expand-file-name "~/libs/plantuml.jar"))

(add-hook 'org-babel-after-execute-hook
          (lambda () (condition-case nil (org-display-inline-images) (error nil)))
          'append)

(setq org-confirm-babel-evaluate
      (lambda (lang body)
        "LANG: , BODY."
        (and (not (string= lang "ditaa"))
             (not (string= lang "dot"))
             (not (string= lang "plantuml"))
             (not (string= lang "gnuplot"))
             )))


(require 'ox-publish)
(setq org-publish-project-alist
      '(("blog-notes"
         :base-directory "~/org/homogenius" ;存放笔记目录
         :base-extension "org"
         :publishing-directory "~/org/homo_public_html/" ; 导出目录
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :auto-preamble t
         :section-numbers t
         :author "gadmyth"
         :email "gadmyth@gmail.com"
         :with-email t
         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-title "Gadmyth Workspace"
         :sitemap-sort-files anti-chronologically
         :sitemap-file-entry-format "%d %t")
        ("blog-static"
         :base-directory "~/org/homogenius"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|ico\\|pdf\\|mp3\\|mp4\\|ogg\\|swf"
         :publishing-directory "~/org/homo_public_html/"
         :recursive t
         :publishing-function org-publish-attachment)
        ("blog" :components ("blog-notes" "blog-static"))
        ))

(defun org-publish-current-file-without-inner-resource-files ()
  "."
  (interactive)
  (let ((org-html-head ""))
    (org-publish-current-file)))

;;; mobile org
;(setq org-mobile-directory "/usr/uploads/")
;(setq org-mobile-use-encryption t)

;;; font, make the org table the right align
(require 'fonts)

(when-font-exist
 "Ubuntu Mono"
 (custom-set-faces `(org-table ((t (:foreground "#6c71c4" :family ,font-name))))))

(provide 'org-config)
;;; org-config.el ends here
