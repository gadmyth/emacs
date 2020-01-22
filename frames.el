;;; package --- frames.el
;;; Commentary:
;;; Code:

(require 'frame)
(require 'async)

(defvar *max-frame-width* 0)
(defvar *max-frame-height* 0)

(defadvice toggle-frame-maximized (before mark-frame-maxsize activate)
  "AFTER: , ACTIVATE: ."
  (message "toggle-frame-maximized advice")
  (async-start
   (lambda ()
     (sleep-for 0.5)
     0.5)
   (lambda (result)
     (message "after toggle-frame-maximized %f seconds" result)
     (let* ((frame (selected-frame))
            (fullscreen-value (frame-parameter frame 'fullscreen)))
       (message "width: %d, height: %d, %s" (frame-width frame) (frame-height frame) fullscreen-value)
       (when (or (eq fullscreen-value 'maximized)
                 (eq fullscreen-value 'fullboth))
         (setq *max-frame-width*  (frame-width frame)
               *max-frame-height* (frame-height frame))
         (message "max-width: %d, max-height: %d" *max-frame-width* *max-frame-height*))))))

(defun set-suitable-frame-size-inner (frame)
  "FRAME: ."
  (set-frame-size
   frame
   (round (* *max-frame-width* 0.5))
   (round (* *max-frame-height* 0.8))))

(defun set-suitable-frame-size ()
  "."
  (interactive)
  (let ((frame (selected-frame)))
    (if (frame-parameter frame 'fullscreen)
        (progn
          (set-frame-parameter frame 'fullscreen nil)
          (async-start
           (lambda ()
             (sleep-for 0.5))
           (lambda (result)
             (set-suitable-frame-size-inner (selected-frame)))))
      (set-suitable-frame-size-inner frame))))

(global-set-key (kbd "C-x 3") (lambda () (interactive) (select-window (split-window-right))))
(if (boundp 'tool-bar-mode) (tool-bar-mode +tool-bar-mode+))
(if (boundp 'menu-bar-mode) (menu-bar-mode +menu-bar-mode+))
(if (boundp 'blink-cursor-mode) (blink-cursor-mode +blink-cursor-mode+))
(if (boundp 'scroll-bar-mode) (scroll-bar-mode +scroll-bar-mode+))
(put 'scroll-left 'disabled nil)


(defun swap-to-main-window ()
  "."
  (interactive)
  (when (>= (count-windows) 2)
	(let* ((first-window (frame-first-window))
		   (first-buffer (window-buffer first-window))
		   (current-window (get-buffer-window))
		   (current-buffer (window-buffer current-window))
		   (first-start (window-start first-window))
		   (current-start (window-start current-window)))
	  (set-window-buffer first-window current-buffer)
	  (set-window-buffer current-window first-buffer)
	  (set-window-start first-window current-start)
	  (set-window-start current-window first-start)
	  (select-window first-window))))

(defun goto-main-window ()
  "."
  (interactive)
  (select-window (frame-first-window)))

(defun goto-next-window()
  "."
  (interactive)
  (other-window +1))

(global-set-key (kbd "C-c C-m") 'goto-main-window)
(global-set-key (kbd "C-c RET") 'swap-to-main-window)
(global-set-key (kbd "C-c C-n") 'goto-next-window)
(global-set-key (kbd "C-c C-f") 'ido-find-file)

(defun get-workspace (index from-end)
  "INDEX: , FROM-END: ."
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
