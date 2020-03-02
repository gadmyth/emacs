(evil-mode 1)
(require 'evil-visualstar)

(setq evil-emacs-state-cursor  '("#ae7865" box))
(setq evil-normal-state-cursor '("gray" box))
(setq evil-visual-state-cursor '("yellow" box))
(setq evil-insert-state-cursor '("gray" hollow))
(setq evil-motion-state-cursor '("red" box))
(define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
(define-key evil-insert-state-map (kbd "C-j") 'evil-complete-next)
(define-key evil-insert-state-map (kbd "C-k") 'evil-complete-previous)
(define-key evil-insert-state-map (kbd "C-h") 'evil-backward-char)
(define-key evil-insert-state-map (kbd "C-l") 'evil-forward-char)
(define-key evil-insert-state-map (kbd "C-n") 'evil-next-line)
(define-key evil-insert-state-map (kbd "C-p") 'evil-previous-line)

(global-set-key (kbd "C-6") 'evil-buffer)

(require-if-installed
 'ace-jump-mode
 (define-key evil-normal-state-map (kbd "SPC") 'ace-jump-word-mode)
 (define-key evil-normal-state-map (kbd "S-SPC") 'ace-jump-char-mode)
 (define-key evil-normal-state-map (kbd "M-SPC") 'ace-jump-line-mode)
 (define-key evil-normal-state-map (kbd "S-M-SPC") 'ace-jump-buffer))

(defun is-char? (c)
  (or (and (<= ?A c) (<= c ?Z))
	  (and (<= ?a c) (<= c ?z))))

(delq nil
	  (mapcar (lambda (x)
		  (if (and
			   (eq 'marker (type-of (cdr x))) ;; this can use markerp
			   (is-char? (car x)))
			x
			nil))
		evil-markers-alist))

(mapc (lambda (hook)
        (add-hook
         hook
         (lambda ()
           (evil-mode)
           (evil-emacs-state))))
      '(slime-repl-mode-hook
        inferior-emacs-lisp-mode-hook
        term-mode-hook
        calendar-mode-hook
        org-agenda-mode-hook
        erc-mode-hook
        ielm-mode-hook
        diff-mode-hook))

;; when back to evil mode we auto save buffer
(defadvice evil-normal-state (after auto-save last activate)
  (when (evil-normal-state-p)
   (if (and (buffer-file-name) (buffer-modified-p))
        (save-buffer)
      ))) 


;; modify evil-show-marks, copy and show the content of the marks
(evil-define-command my-evil-show-marks (mrks)
  "Shows all marks.
If MRKS is non-nil it should be a string and only registers
corresponding to the characters of this string are shown."
  :repeat nil
  (interactive "<a>")
  ;; To get markers and positions, we can't rely on 'global-mark-ring'
  ;; provided by Emacs (although it will be much simpler and faster),
  ;; because 'global-mark-ring' does not store mark characters, but
  ;; only buffer name and position. Instead, 'evil-markers-alist' is
  ;; used; this is list maintained by Evil for each buffer.
  (let ((all-markers
         ;; get global and local marks
         (append (cl-remove-if (lambda (m)
                                 (or (evil-global-marker-p (car m))
                                     (not (markerp (cdr m)))))
                               evil-markers-alist)
                 (cl-remove-if (lambda (m)
                                 (or (not (evil-global-marker-p (car m)))
                                     (not (markerp (cdr m)))))
                               (default-value 'evil-markers-alist)))))
    (when mrks
      (setq mrks (string-to-list mrks))
      (setq all-markers (cl-delete-if (lambda (m)
                                        (not (member (car m) mrks)))
                                      all-markers)))
    ;; map marks to list of 4-tuples (char row col file)
    (setq all-markers
          (mapcar (lambda (m)
                    (with-current-buffer (marker-buffer (cdr m))
                      (save-excursion
                        (goto-char (cdr m))
                        (beginning-of-line)
                        (list (car m)
                              (line-number-at-pos (point))
                              (string-trim (buffer-substring (point) (line-end-position)))
                              (buffer-name)))))
                  all-markers))
    (evil-with-view-list
      :name "evil-marks"
      :mode-name "Evil Marks"
      :format [("Mark" 8 nil)
               ("Line" 8 nil)
               ("Content" 40 nil)
               ("Buffer" 1000 nil)]
      :entries (cl-loop for m in (sort all-markers (lambda (a b) (< (car a) (car b))))
                        collect `(nil [,(char-to-string (nth 0 m))
                                       ,(number-to-string (nth 1 m))
                                       ,(nth 2 m)
                                       (,(nth 3 m))]))
      :select-action #'my-evil--show-marks-select-action)))


(defun ivy-evil-show-marks ()
  "Show evil-marks with ivy-read."
  (interactive)
  (let ((all-markers
         ;; get global and local marks
         (append (cl-remove-if (lambda (m)
                                 (or (evil-global-marker-p (car m))
                                     (not (markerp (cdr m)))))
                               evil-markers-alist)
                 (cl-remove-if (lambda (m)
                                 (or (not (evil-global-marker-p (car m)))
                                     (not (markerp (cdr m)))))
                               (default-value 'evil-markers-alist)))))
    ;; map marks to list of 4-tuples (char row col file)
    (setq all-markers
          (mapcar (lambda (m)
                    (with-current-buffer (marker-buffer (cdr m))
                      (save-excursion
                        (goto-char (cdr m))
                        (beginning-of-line)
                        (list (car m)
                              (line-number-at-pos (point))
                              (string-trim (buffer-substring (point) (line-end-position)))
                              (buffer-name)))))
                  all-markers))
    ;; map marks to ivy-marks
    (setq all-markers
          (mapcar (lambda (m)
                      (let* ((mark (car m))
                             (line-number (cadr m))
                             (content (caddr m))
                             (buffername (cadddr m))
                             (mark-show-line (format "%c\t\t%s\t\t%s" mark content buffername)))
                      (list mark-show-line m)))
                  all-markers))

    (ivy-read "goto evil marks: " all-markers
              :initial-input "^"
              :action (lambda (result)
                        (let ((marker (cadr result)))
                          (evil-goto-mark (car marker)))))))


(defun my-evil--show-marks-select-action (entry)
  (kill-this-buffer)
  (delete-window)
  (switch-to-buffer (car (elt entry 3)))
  (evil-goto-mark (string-to-char (elt entry 0))))

(define-key evil-list-view-mode-map (kbd "C-g") #'evil-list-view-quit)
(defun evil-list-view-quit ()
  (interactive)
  (kill-this-buffer)
  (delete-window))

(provide 'evil-config)