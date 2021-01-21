;;; package --- windows.el
;;; Commentary:
;;; Code:

(require 'windmove)
(windmove-default-keybindings)

;(require 'window-numbering)
;(window-numbering-mode 1)

(require 'winner)
(winner-mode 1)
(global-set-key (kbd "<H-up>") #'winner-undo)
(global-set-key (kbd "<H-down>") #'winner-redo)

;; set mouse autoselect window
(setq mouse-autoselect-window t)

;; adjust window size
(global-set-key (kbd "H-h") #'enlarge-window-horizontally)
(global-set-key (kbd "H-l") #'shrink-window-horizontally)

;; window operation
(global-set-key (kbd "C-x 2") #'split-window-below-with-ratio)
(global-set-key (kbd "C-x 3") #'split-window-right-with-ratio)
(global-set-key (kbd "H-m") 'goto-main-window)
(global-set-key (kbd "H-s") 'swap-window-in-current-frame)
(global-set-key (kbd "H-c") 'copy-window-in-current-frame)
(global-set-key (kbd "<H-return>") 'swap-to-main-window)
(global-set-key (kbd "H-k") #'goto-previous-window)
(global-set-key (kbd "H-j") #'goto-next-window)
(global-set-key (kbd "<H-tab>") #'goto-next-window)
(global-set-key (kbd "C-c C-f") 'ido-find-file)

(defun split-window-below-with-ratio ()
  "."
  (interactive)
  (let* ((height (window-height (get-buffer-window)))
         (ratio (string-to-number
                 (completing-read "Select the split ratio: " '("2" "3" "4" "5") nil t)))
         (top-ratio (- 1 (/ 1.0 ratio)))
         (top-window-height (round (* height top-ratio))))
    (select-window (split-window-below top-window-height))
    (switch-to-buffer (get-buffer-create "*scratch*"))))

(defun split-window-right-with-ratio ()
  "."
  (interactive)
  (let* ((width (window-width (get-buffer-window)))
         (ratio (string-to-number
                 (completing-read "Select the split ratio: " '("2" "3" "4" "5") nil t)))
         (left-ratio (- 1 (/ 1.0 ratio)))
         (left-window-width (round (* width left-ratio))))
    (select-window (split-window-right left-window-width))
    (switch-to-buffer (get-buffer-create "*scratch*"))))

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

(defun goto-next-window()
  "."
  (interactive)
  (select-window (next-window)))

(defun goto-previous-window()
  "."
  (interactive)
  (select-window (previous-window)))

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
      (if (and (string-prefix-p "*" name)
               (let ((first-char (substring name 1 2)))
                 (equal (upcase first-char) first-char)))
          (if (window-live-p window)
              (quit-window nil window))))))

(defun quit-temp-windows ()
  "."
  (interactive)
  (if (active-minibuffer-window)
      (minibuffer-keyboard-quit)
    (quit-help-windows)))

(global-set-key (kbd "C-x 9") 'delete-other-windows-of-super-window)
(global-set-key (kbd "C-x q") 'quit-temp-windows)

(provide 'windows)
;;; windows.el ends here
