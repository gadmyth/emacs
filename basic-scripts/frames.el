;;; package --- frames.el
;;; Commentary:
;;; Code:

(require 'frame)
(require 'async)
(require 'eyebrowse+)

(defvar *max-frame-width* 0)
(defvar *max-frame-height* 0)

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

(defvar *tool-bar-mode* t)
(defvar *menu-bar-mode* t)
(defvar *blink-cursor-mode* t)
(defvar *scroll-bar-mode* t)

(if (boundp 'tool-bar-mode) (tool-bar-mode *tool-bar-mode*))
(if (boundp 'menu-bar-mode) (menu-bar-mode *menu-bar-mode*))
(if (boundp 'blink-cursor-mode) (blink-cursor-mode *blink-cursor-mode*))
(if (boundp 'scroll-bar-mode) (scroll-bar-mode *scroll-bar-mode*))
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
