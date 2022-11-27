;;; package --- windows.el
;;; Commentary:
;;; Code:

(require-package
 'windmove
 (windmove-default-keybindings))

(require-package
 'window-numbering
 (window-numbering-mode 1))

(require-package
 'winner
 (winner-mode 1)
 (global-set-key (kbd "<H-up>") #'winner-undo)
 (global-set-key (kbd "<H-down>") #'winner-redo))

(require 'cl-seq)

;; set mouse autoselect window
(setq mouse-autoselect-window t)

;; window operation
;; originally bind to C-x 2, also overwrite the split-window key binding
(global-set-key (kbd "C-2") #'split-window-below-with-ratio)
;; originally bind to C-x 3, also overwrite the split-window-vertically key binding
(global-set-key (kbd "C-3") #'split-window-right-with-ratio)
;; originally bind to C-x C-2
(global-set-key (kbd "C-M-2") #'(lambda () (interactive) (split-window-below-with-ratio t)))
;; originally bind to C-x C-3
(global-set-key (kbd "C-M-3") #'(lambda () (interactive) (split-window-right-with-ratio t)))
(global-set-key (kbd "C-c C-f") 'ido-find-file)
(global-set-key (kbd "H-e") 'scroll-line-up)
(global-set-key (kbd "H-y") 'scroll-line-down)

(defun scroll-line-up (count)
  "Scrolls the window COUNT lines upwards."
  "Copied from evil-commands.el."
  (interactive "p")
  (let ((scroll-preserve-screen-position nil))
    (scroll-up count)))

(defun scroll-line-down (count)
  "Scrolls the window COUNT lines forwards."
  "Copied from evil-commands.el."
  (interactive "p")
  (let ((scroll-preserve-screen-position nil))
    (scroll-down count)))

(defun split-window-below-with-ratio (&optional switch-to-scratch-buffer-p)
  "Split window below with ratio that the user selected, if SWITCH-TO-SCRATCH-BUFFER-P, change the new buffer to *scratch* buffer."
  (interactive)
  (split-window-with-ratio 'above 'below switch-to-scratch-buffer-p))

(defun split-window-right-with-ratio (&optional switch-to-scratch-buffer-p)
  "Split window right with ratio that the user selected, if SWITCH-TO-SCRATCH-BUFFER-P, change the new buffer to *scratch* buffer."
  (interactive)
  (split-window-with-ratio 'left 'right switch-to-scratch-buffer-p))

(defun split-window-with-ratio (side1 side2 &optional switch-to-scratch-buffer-p)
  "Split buffer default on SIDE2, if ctrl-u is pressed, split the new window on SIDE1."
  (let* ((split-another-side (not (null current-prefix-arg)))
         (side (if split-another-side side1 side2))
         (full-window-size (if (eq side1 'left)
                               (window-width (get-buffer-window))
                             (window-height (get-buffer-window))))
         (ratio (string-to-number
                 (completing-read "Select the split ratio: " '("0.5" "0.8" "0.75" "0.618" "0.33" "0.25" "0.2" "0.1") nil t)))
         (split-window-size (round (* full-window-size ratio))))
    (select-window (split-window nil (* -1 split-window-size) side))
    (if switch-to-scratch-buffer-p
        (switch-to-buffer (get-buffer-create "*scratch*")))))

(defun swap-to-main-window ()
  "."
  (interactive)
  (swap-window (frame-first-window)))

(defun swap-window-old (window)
  "Swap the current window with WINDOW."
  (interactive)
  (when (>= (count-windows) 2)
    (let* ((first-buffer (window-buffer window))
           (current-window (get-buffer-window))
           (current-buffer (window-buffer current-window))
           (first-start (window-start window))
           (current-start (window-start current-window)))
      (set-window-buffer window current-buffer)
      (set-window-buffer current-window first-buffer)
      (set-window-start window current-start)
      (set-window-start current-window first-start)
      (select-window window))))

(defun swap-window (window)
  "Swap the current window with WINDOW."
  (interactive)
  (when (and (>= (count-windows) 2)
             (window-live-p window))
    (let* ((current-window (get-buffer-window)))
      (window-swap-states current-window window))))

(defun goto-main-window ()
  "."
  (interactive)
  (select-window (frame-first-window)))

(defun swap-window-at-index (index)
  "Swap the current window the window at INDEX.
Copied some codes from window-numbering.el."
  (interactive "nPlease choose a window number: ")
  (let* ((windows (car (gethash (selected-frame) window-numbering-table)))
         (window (and (>= index 0) (< index 10) (aref windows index))))
    (when window
      (swap-window window))))

(defun goto-next-window ()
  "."
  (interactive)
  (select-window (next-window)))

(defun goto-previous-window ()
  "."
  (interactive)
  (select-window (previous-window)))

(defun goto-last-window ()
  "."
  (interactive)
  (let ((current-window (get-buffer-window))
        (target-window (get-mru-window t t t)))
    (when (not (equal current-window target-window))
      (message "goto-last-window: %S" target-window)
      (let ((current-frame (window-frame current-window))
            (target-frame (window-frame target-window)))
        (when (not (equal current-frame target-frame))
          (x-focus-frame target-frame))
        (select-window target-window)))))

(defun swap-window-in-current-frame ()
  "."
  (interactive)
  (let* ((current-window (get-buffer-window (current-buffer)))
         (windows (delq current-window (window-list)))
         (cands (mapcar (lambda (window)
                          (cons (buffer-name (window-buffer window)) window))
                        windows))
         (preselect (caar cands)))
    (ivy-read
     "Select the buffer to swap: " cands
     :preselect preselect
     :action
     (lambda (cand)
       (if (listp cand)
           (swap-window (cdr cand)))))))

(defun copy-window-in-current-frame ()
  "."
  (interactive)
  (let* ((buffer (current-buffer))
         (current-window (get-buffer-window buffer))
         (windows (delq current-window (window-list)))
         (cands (mapcar (lambda (window)
                          (cons (buffer-name (window-buffer window)) window))
                        windows))
         (preselect (caar cands)))
    (ivy-read
     "Select the buffer to copy: " cands
     :preselect preselect
     :action
     (lambda (cand)
       (when (listp cand)
         (select-window (cdr cand))
         (switch-to-buffer buffer))))))

(defmacro horizontal-delta (direction)
  "DIRECTION."
  `(cond
    ;; right side can adjust
    ((not (window-at-side-p nil 'right))
     (if (equal ,direction 'left) -5 5))
    ;; left side can adjust
    ((not (window-at-side-p nil 'left))
     (if (equal ,direction 'left) 5 -5))))

(defmacro vertical-delta (direction)
  "DIRECTION."
  `(cond
    ;; down side can adjust
    ((not (window-at-side-p nil 'bottom))
     (if (equal ,direction 'down) 5 -5))
    ((not (window-at-side-p nil 'top))
     (if (equal ,direction 'down) -5 5))))

(defun adjust-window-size ()
  "."
  (interactive)
  (let* ((ev last-command-event)
         (base (event-basic-type ev)))
    (message "base: %S" base)
    (pcase base
      (?\[ (enlarge-window-horizontally (horizontal-delta 'left)))
      (?\] (enlarge-window-horizontally (horizontal-delta 'right)))
      ('left (enlarge-window-horizontally (horizontal-delta 'left)))
      ('right (enlarge-window-horizontally (horizontal-delta 'right)))
      (?5 (enlarge-window (vertical-delta 'down)))
      (?6 (enlarge-window (vertical-delta 'up)))
      ('down (enlarge-window (vertical-delta 'down)))
      ('up (enlarge-window (vertical-delta 'up)))
      (_ nil)))
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (vector (append '(control) (list ?\[)))
       (lambda () (interactive) (adjust-window-size)))
     (define-key map (vector (append '(control) (list ?\])))
       (lambda () (interactive) (adjust-window-size)))
     (define-key map (vector (append '(control) (list ?6)))
       (lambda () (interactive) (adjust-window-size)))
     (define-key map (vector (append '(control) (list ?5)))
       (lambda () (interactive) (adjust-window-size)))
     map)))

;; adjust window size
(global-set-key (kbd "C-x C-]") 'adjust-window-size)
(global-set-key (kbd "C-x C-[") 'adjust-window-size)
(global-set-key (kbd "C-x C-6") 'adjust-window-size)
(global-set-key (kbd "C-x C-5") 'adjust-window-size)


(defun delete-other-windows-of-super-window (&optional window)
  "WINDOW."
  (interactive)
  (let* ((window (or window (get-buffer-window)))
         (super-window (window-parent window))
         child)
    (when (and (windowp super-window)
               (setq child (window-child super-window)))
      (while child
        (let ((w child))
          (setq child (window-next-sibling window))
          (if (not (eq window w))
              (delete-window w)))))))

(defun quit-help-window ()
  "."
  (interactive)
  (let ((window (get-buffer-window "*Help*")))
    (if (window-live-p window)
        (quit-window nil window))))
  
(defun quit-help-windows ()
  "."
  (dolist (window (window-list))
    (let* ((buffer (window-buffer window))
           (name (buffer-name buffer)))
      (if (string-prefix-p "*" (string-trim name))
          (if (window-live-p window)
              (quit-window nil window))))))

(defun quit-temp-windows ()
  "."
  (interactive)
  (if (active-minibuffer-window)
      (minibuffer-keyboard-quit)
    (quit-help-windows)))

(defun run-or-raise-next (&optional buffer-filter run-func)
  "Run or raise next buffer in the buffer list filtered by BUFFER-FILTER, if buffer list is empty, exec the RUN-FUNC to create one."
  (let* ((buffers (seq-filter (or buffer-filter #'identity) (buffer-list)))
         (buffers (sort buffers (lambda (a b) (string< (buffer-name a) (buffer-name b))))))
    (if (> (length buffers) 0)
        (let* ((len (length buffers))
               (position (cl-position (current-buffer) buffers :test #'equal))
               (index (if position (% (+ position 1) len) 0))
               (target (nth index buffers)))
          (message "len: %S, position: %S, index: %S, target: %S" len position index target)
          (switch-to-buffer target))
      (call-interactively run-func))))

(defun run-or-raise-next-terminal ()
  "."
  (interactive)
  (run-or-raise-next
   (lambda (buffer)
     (let ((mode (with-current-buffer buffer major-mode)))
       (or (eq 'term-mode mode)
           (eq 'shell-mode mode)
           (eq 'eshell-mode mode))))
   (lambda ()
     (interactive)
     ;; TODO: bash path in different OS, or use multi-term
     (term "/bin/bash"))))

(defun switch-to-scratch-buffer ()
  "."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))

(defun switch-to-message-buffer ()
  "."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Messages*")))

;; bind keymap additionally, originally bind to C-x 1
(global-set-key (kbd "C-1") 'delete-other-windows)
;; bind keymap additionally, originally bind to C-x 0
(global-set-key (kbd "C-0") 'delete-window)
;; originally bind to C-x 9
(global-set-key (kbd "C-9") 'delete-other-windows-of-super-window)
(global-set-key (kbd "C-x q") 'quit-temp-windows)

(provide 'windows)
;;; windows.el ends here
