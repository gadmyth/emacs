;;; org-config.el --- config org-mode
(require 'org)

;;; todo-keywords
(setq org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "CLOSING(c)" "PENDING(p)" "PLAN(l)" "|" "DONE(d!)" "ABORT(a@/!)")))

(setq org-todo-keyword-faces '(("PENDING" . (:background "LightGreen" :foreground "black" :weight bold))
                               ("PLAN" . (:background "LightGray" :foreground "white" :weight bold))
                               ("DOING" . (:background "Green" :foreground "white" :weight bold))
                               ("CLOSING" . (:background "Orange" :foreground "white" :weight bold))
                               ("TODO" . (:background "DarkOrange" :foreground "black" :weight bold))
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
(setq org-agenda-files '("~/org"))
(setq org-agenda-ndays 14)
(setq org-agenda-include-diary t)
(global-set-key (kbd "C-c a") 'org-agenda)
;(global-set-key (kbd "<f6>") 'org-todo-list)

;;; capture
(setq org-default-notes-file (concat org-directory "/notes.org"))
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file+datetree "~/org/task.org")
         "* TODO %^{Decription} %^T %^g\n %i")
        ("T" "Timeline" entry (file+datetree "~/org/timeline.org")
         "* %^{Decription} %^T %^g\n %i")
        ("e" "event" entry (file+datetree "~/org/timeline.org")
         "* %^{Decription} %T :event:\n %i")
        ("r" "routine" entry (file+datetree "~/org/timeline.org")
         "* %^{Decription} %T :routine:\n %i")
        ("p" "problem" entry (file+datetree "~/org/timeline.org")
         "* %^{Decription} %T :problem:\n %i")
        ("P" "plan" entry (file+datetree "~/org/timeline.org")
         "* %^{Decription} %T :plan:\n %i")
        ("s" "summary" entry (file+datetree "~/org/timeline.org")
         "* %^{Decription} %T :summary:\n %i")
        ("n" "Note" entry (file+headline "~/org/notes.org" "Notes")
         "* %A %^g %i")
        ("f" "Someday" entry (file+headline "~/org/task.org" "Someday")
         "* %?")
        ))

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
             (mapcar #'(lambda (file)
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
  (case checkbox (on "<span class=\"check\">&#x2611;</span>") ; checkbox (checked)
        (off "<span class=\"checkbox\">&#x2610;</span>")
        (trans "<span class=\"checkbox\">&#x229F;")
        (t "")))

(defadvice org-html-checkbox (around sacha activate)
  (setq ad-return-value (m/org-html-checkbox (ad-get-arg 0))))

(setq org-export-preserve-breaks "<br>")

(eval-after-load "org"
  '(progn
	 (setq org-startup-indented t)
     (setq org-startup-folded "showall")))

;;; setup org src code color
(setq org-src-fontify-natively t)

;;; src block indent
(setq org-edit-src-content-indentation 0)

;;; inline image size
(setq org-image-actual-width nil)

;;; superscripts
(setq-default org-use-sub-superscripts '{})
(setq-default org-export-with-sub-superscripts '{})

;;; underline
(setq-default org-export-with-emphasize nil)

;;; babel language
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t)
   (plantuml . t)
   (dot . t)
   (restclient . t)
   (shell . t)
   (ruby . t)
   (python . t)
   (gnuplot . t)
   ;(jq . t)
   ))

;;; headline levels
(setq org-export-headline-levels 5)
;;; toc level
(setq org-export-with-toc 5)

(setq org-ditaa-jar-path (expand-file-name "~/libs/ditaa.jar"))
(setq org-plantuml-jar-path (expand-file-name "~/libs/plantuml.jar"))

(add-hook 'org-babel-after-execute-hook
          '(lambda () (condition-case nil (org-display-inline-images) (error nil)))
          'append)

(setq org-confirm-babel-evaluate
      '(lambda (lang body)
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


(defvar *org-cap-temp*)
(defun org-capture-current-line (description)
  "DESCRIPTION: ."
  (interactive "sSet the line description here: ")
  (re-search-backward "^" nil t)
  (re-search-forward "^ *\\(.*?\\)$" nil t 1)
  (let* ((line (match-string-no-properties 1))
         (encoded-line (url-encode-url line))
         (formatted-line (format "[[file:%s::%s][%s]]" (buffer-file-name) encoded-line description)))
    (setq *org-cap-temp* formatted-line)
    (visit-work-file)))

(defun org-capture-insert-temp ()
  "."
  (interactive)
  (if (not (null *org-cap-temp*))
      (progn
        (insert-string *org-cap-temp*)
        (setq *org-cap-temp* nil))))

(defun org-capture-dired-file (description)
  "DESCRIPTION: ."
  (interactive "sSet the dir description here: ")
  (let* ((file (dired-get-file-for-visit))
        (formatted-line (format "[[file:%s][%s]]" file description)))
    (setq *org-cap-temp* formatted-line)
    ;(visit-work-file)
    ))

(defun org-parse-file-link ()
  "."
  (interactive)
  (save-excursion
    (re-search-backward "^" nil t)
    (if (re-search-forward "^.*?file:\\([^]:]*\\).*$" nil t 1)
        (let* ((org-link-file (match-string-no-properties 1))
               (org-link-dir (if (file-directory-p org-link-file)
                                 org-link-file
                               (file-name-directory org-link-file))))
          (cons org-link-file org-link-dir))
      (cons nil nil))))

(defun org-open-dir ()
  "."
  (interactive)
  (let* ((parse-result (org-parse-file-link))
         (file (car parse-result))
         (dir (cdr parse-result)))
    (when (and (not (null file))
               (not (null dir)))
      (dired-other-window dir)
      (dired-goto-file file)
      (message "Selected file is: %s" file))))

(defun org-show-link ()
  "."
  (interactive)
  (let* ((parse-result (org-parse-file-link))
         (file (car parse-result))
         (dir (cdr parse-result)))
    (if (and (not (null file)) (not (null dir)))
      (message "The file of link is: %s" file))))

(defun org-copy-link ()
  "."
  (interactive)
  (save-excursion
    (re-search-backward "^" nil t)
    (if (re-search-forward "^.*?\\(https?:[^]]*\\).*$" nil t 1)
        (let* ((org-link (match-string-no-properties 1)))
          (kill-new org-link)
          (message org-link)
          org-link))))

(global-set-key (kbd "<f7>") 'org-capture-current-line)
(global-set-key (kbd "<f8>") 'org-capture-insert-temp)
(global-set-key (kbd "C-c d") 'org-open-dir)
(global-set-key (kbd "C-c P") 'org-show-link)

;;; font, make the org table the right align
(require 'fonts)

(when-font-exist
 "Ubuntu Mono"
 (custom-set-faces `(org-table ((t (:foreground "#6c71c4" :family ,font-name))))))

(provide 'org-config)
;;; org-config.el ends here
