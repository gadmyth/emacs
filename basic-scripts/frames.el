;;; package --- frames.el
;;; Commentary:
;;; Code:

(require 'frame)
(require 'eyebrowse+)


(setq frame-title-format
      '(:eval
        (let* ((buffer (current-buffer))
               (buffer-name (buffer-name buffer))
               (file-name (buffer-file-name buffer))
               (locked-conf (and (boundp '*eyebrowse-locked-config*)
                                 (eyebrowse-config-string *eyebrowse-locked-config*)))
               (eb-conf (eyebrowse-current-config-string))
               (last-conf (eyebrowse-config-string (eyebrowse-get-last-config))))
          ;; show file name first, if nil show buffer name; and also show the buffer-locked and current eyebrowse config
          (format "%s - [%s, %s, %s]" (or file-name buffer-name)
                  locked-conf eb-conf last-conf))))

(defun set-suitable-frame-size-inner (frame)
  "FRAME: ."
  (set-frame-size
   frame
   (round (* (display-pixel-width) 0.5))
   (round (* (display-pixel-height) 0.8))
   t))

(defun set-suitable-frame-size ()
  "."
  (interactive)
  (let ((frame (selected-frame)))
    (set-frame-parameter frame 'fullscreen nil)
    (set-suitable-frame-size-inner (selected-frame))))

(defun scale-frame-size (frame scale)
  "FRAME: , SCALE."
  (set-frame-size
   frame
   (round (* (frame-parameter frame 'width) scale))
   (round (* (frame-parameter frame 'height) scale))))

(defun adjust-frame-horizontally (frame delta)
  "FRAME: , DELTA."
  (set-frame-width
   frame
   (+ (frame-parameter frame 'width) delta)))

(defun adjust-frame-vertically (frame delta)
  "FRAME: , DELTA."
  (set-frame-height
   frame
   (+ (frame-parameter frame 'height) delta)))

(defun scale-frame-horizontally (frame scale)
  "FRAME: , SCALE."
  (set-frame-width
   frame
   (round (* (frame-parameter frame 'width) scale))))

(defun scale-frame-vertically (frame scale)
  "FRAME: , SCALE."
  (set-frame-height
   frame
   (round (* (frame-parameter frame 'height) scale))))

(defun adjust-frame-size ()
  "."
  (interactive)
  (let* ((ev last-command-event)
         (modifiers (event-modifiers ev))
         (scale-p (memq 'shift modifiers))
         (base (event-basic-type ev))
         (frame (selected-frame)))
    (cond
     (scale-p
      (pcase base
        ('right (scale-frame-horizontally frame 2))
        ('left (scale-frame-horizontally frame 0.5))
        ('down (scale-frame-vertically frame 2))
        ('up (scale-frame-vertically frame 0.5))
        (_ nil)))
     (t
      (pcase base
        ('right (adjust-frame-horizontally frame 5))
        ('left (adjust-frame-horizontally frame -5))
        ('down (adjust-frame-vertically frame 5))
        ('up (adjust-frame-vertically frame -5))
        (_ nil))))
    (message "use ctrl-left, ctrl-right, ctrl-up, ctrl-down for further adjustment")
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map (vector (append '(control) (list 'left)))
         #'adjust-frame-size)
       (define-key map (vector (append '(control) (list 'right)))
         #'adjust-frame-size)
       (define-key map (vector (append '(control) (list 'up)))
         #'adjust-frame-size)
       (define-key map (vector (append '(control) (list 'down)))
         #'adjust-frame-size)
       (define-key map (vector (append '(control shift) (list 'left)))
         #'adjust-frame-size)
       (define-key map (vector (append '(control shift) (list 'right)))
         #'adjust-frame-size)
       (define-key map (vector (append '(control shift) (list 'up)))
         #'adjust-frame-size)
       (define-key map (vector (append '(control shift) (list 'down)))
         #'adjust-frame-size)
       map))))

(defun move-frame-horizontally (frame delta)
  "FRAME: , DELTA."
  (set-frame-parameter
   frame
   'left
   (+ (frame-parameter frame 'left) delta)))

(defun move-frame-vertically (frame delta)
  "FRAME: , DELTA."
  (set-frame-parameter
   frame
   'top
   (+ (frame-parameter frame 'top) delta)))

(defun adjust-frame-position ()
  "."
  (interactive)
  (let* ((ev last-command-event)
         (modifiers (event-modifiers ev))
         (scale-p (memq 'shift modifiers))
         (base (event-basic-type ev))
         (frame (selected-frame)))
    (cond
     (scale-p
      (pcase base
        ('right (move-frame-horizontally frame 20))
        ('left (move-frame-horizontally frame -20))
        ('down (move-frame-vertically frame 20))
        ('up (move-frame-vertically frame -20))
        (_ nil)))
     (t
      (pcase base
        ('right (move-frame-horizontally frame 5))
        ('left (move-frame-horizontally frame -5))
        ('down (move-frame-vertically frame 5))
        ('up (move-frame-vertically frame -5))
        (_ nil))))
    (message "use meta-left, meta-right, meta-up, meta-down for further adjustment")
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map (vector (append '(meta) (list 'left)))
         #'adjust-frame-position)
       (define-key map (vector (append '(meta) (list 'right)))
         #'adjust-frame-position)
       (define-key map (vector (append '(meta) (list 'up)))
         #'adjust-frame-position)
       (define-key map (vector (append '(meta) (list 'down)))
         #'adjust-frame-position)
       (define-key map (vector (append '(meta shift) (list 'left)))
         #'adjust-frame-position)
       (define-key map (vector (append '(meta shift) (list 'right)))
         #'adjust-frame-position)
       (define-key map (vector (append '(meta shift) (list 'up)))
         #'adjust-frame-position)
       (define-key map (vector (append '(meta shift) (list 'down)))
         #'adjust-frame-position)
       map))))

(global-set-key (kbd "C-x C-<left>") 'adjust-frame-size)
(global-set-key (kbd "C-x C-<right>") 'adjust-frame-size)
(global-set-key (kbd "C-x C-<up>") 'adjust-frame-size)
(global-set-key (kbd "C-x C-<down>") 'adjust-frame-size)
(global-set-key (kbd "C-x C-S-<left>") 'adjust-frame-size)
(global-set-key (kbd "C-x C-S-<right>") 'adjust-frame-size)
(global-set-key (kbd "C-x C-S-<up>") 'adjust-frame-size)
(global-set-key (kbd "C-x C-S-<down>") 'adjust-frame-size)

(global-set-key (kbd "C-x M-<left>") 'adjust-frame-position)
(global-set-key (kbd "C-x M-<right>") 'adjust-frame-position)
(global-set-key (kbd "C-x M-<up>") 'adjust-frame-position)
(global-set-key (kbd "C-x M-<down>") 'adjust-frame-position)
(global-set-key (kbd "C-x M-S-<left>") 'adjust-frame-position)
(global-set-key (kbd "C-x M-S-<right>") 'adjust-frame-position)
(global-set-key (kbd "C-x M-S-<up>") 'adjust-frame-position)
(global-set-key (kbd "C-x M-S-<down>") 'adjust-frame-position)


(if (boundp 'tool-bar-mode) (tool-bar-mode (bound-or-default *tool-bar-mode* t)))
(if (boundp 'menu-bar-mode) (menu-bar-mode (bound-or-default *menu-bar-mode* t)))
(if (boundp 'blink-cursor-mode) (blink-cursor-mode (bound-or-default *blink-cursor-mode* t)))
(if (boundp 'scroll-bar-mode) (scroll-bar-mode (bound-or-default *scroll-bar-mode* t)))

(put 'scroll-left 'disabled nil)

(defun get-workspace (index from-end)
  "Get frame by INDEX, FROM-END means index from the end of frames list."
  "Workspace is frame."
  (let* ((frame-num (length (frame-list)))
         (max-index (- frame-num 1))
         (frame-index (if (not from-end)
                          index
                        (- max-index index))))
    (nth frame-index (frame-list))))

(defun goto-workspace-by-number (index)
  "INDEX: ."
  (if (<= index (- (length (frame-list)) 1))
      (select-frame-set-input-focus (get-workspace index t))
    (message "No workspace found")))

(dotimes (i 10)
  (eval `(defun ,(intern (format "goto-workspace-%s" i)) ()
           ,(format "goto workspace with number %i." i)
           (interactive)
           (goto-workspace-by-number ,(- i 1))))
  (global-set-key (kbd (format "C-c C-%s" i)) (intern (format "goto-workspace-%s" i))))


(provide 'frames)
;;; frames.el ends here
