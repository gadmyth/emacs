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
