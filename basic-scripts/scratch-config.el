;;; package --- scratch-config.el
;;; Commentary:
;;; Code:

(require 'async-config)

(defvar +default-scratch-file-name+ (expand-file-name "~/.emacs.scratch"))

(defvar *last-scratch-buffer-size* -1)

(defvar *scratch-autosave-process* nil)

(defconst +scratch-autosave-interval+ (* 10 60))

(defvar *scratch-autosave-status* nil)

(defvar *scratch-last-saved-time* nil)

(add-to-list 'auto-coding-alist '("\\.scratch\\'" . utf-8))

(defun scratch-process-status ()
  "."
  (let* ((process *scratch-autosave-process*)
         (status (if process (process-status process) nil)))
    status))

(defun scratch-process-running-p ()
  "."
  (let ((status (scratch-process-status)))
    (eq status 'run)))

(defun scratch-autosave-saving-p ()
  "."
  (eq *scratch-autosave-status* 'saving))

(defun scratch-autosave-loading-p ()
  "."
  (eq *scratch-autosave-status* 'loading))

(defun scratch-autosave-init-p ()
  "."
  (eq *scratch-autosave-status* nil))

(defun scratch-reset-autosave-status ()
  "."
  (setq *scratch-autosave-status* nil))

(defun scratch-buffer-size ()
  "."
  (let* ((buffer (get-buffer "*scratch*"))
         (size (buffer-size buffer)))
    size))

(defun scratch-mark-buffer-size ()
  "."
  (setq *last-scratch-buffer-size* (scratch-buffer-size)))

(defun scratch-buffer-size-changed-p ()
  "."
  (let* ((size (scratch-buffer-size))
         (size-changed-p (not (eq *last-scratch-buffer-size* size))))
    size-changed-p))

(defun load-scratch-from-file (&optional override)
  "Load scratch file to *scratch* buffer, if OVERRIDE is t, erase buffer first."
  (interactive)
  (let ((loading-success-p))
    (cond
     ;; don't load while saving
     ((scratch-autosave-saving-p)
      (message "Can't load %s file, for now is saving *scratch* buffer to it!" +default-scratch-file-name+)
      nil)
     ;; don't load again while loading file
     ((scratch-autosave-loading-p)
      (message "Can't load %s file, for now is already loading into *scratch* buffer!" +default-scratch-file-name+)
      nil)
     ;; check the file exists or not
     ((not (file-exists-p +default-scratch-file-name+))
      (message "Can't load %s file, for it does not exist!" +default-scratch-file-name+)
      ;; set loading to success at the very beginning
      (setq loading-success-p t))
     (t
      (let ((buffer (get-buffer-create "*scratch*")))
        ;; lock the status to loading
        (setq *scratch-autosave-status* 'loading)
        (with-current-buffer
            buffer
          (if override (erase-buffer))
          (insert-file-contents +default-scratch-file-name+))
        ;; unlock the status
        (scratch-reset-autosave-status)
        ;; mark buffer size
        (scratch-mark-buffer-size)
        ;; set the loading result to success
        (setq loading-success-p t))))
    ;; return the loading result
    loading-success-p))

(defun start-scratch-autosave-scheduler ()
  "Start a scheduler to auto save scratch buffer."
  (if (scratch-process-running-p)
      (message "*scratch-autosave-process* <%s, %S> started, don't start again!"
               (process-name *scratch-autosave-process*)
               (process-id *scratch-autosave-process*))
    (scratch-reset-autosave-status)
    (setq *scratch-autosave-process* (start-scratch-autosave-process))))

(defun start-scratch-autosave-process ()
  "."
  (message "**** starting *scratch-autosave-process* [%s] ****" (scratch-process-status))
  (call-after
   600
   ;+scratch-autosave-interval+
   (message "*scratch-autosave-process* status: %S" (scratch-process-status))
   (save-scratch-buffer)
   ;; start next scheduler after saving
   (start-scratch-autosave-scheduler)))

(defun save-scratch-buffer ()
  "Save *scatch* buffer to file."
  (interactive)
  (let ((now (format-time-string "%Y-%m-%d %a %H:%M" (current-time)))
        (last-saved-time *scratch-last-saved-time*)
        (buffer (get-buffer "*scratch*")))
    (cond
     ;; don't save when saving to file
     ((scratch-autosave-saving-p)
      (message "Can't save *scratch*, for now is saving it to %s [%s]!" +default-scratch-file-name+ now))
     ;; don't save when loading file
     ((scratch-autosave-loading-p)
      (message "Can't save *scratch*, for now is loading %s into it [now]!" +default-scratch-file-name+ now))
     ;; check the buffer exists or not
     ((not buffer)
      (message "buffer <*scratch*> does not exist!"))
     ;; check the buffer changed or not
     ((not (scratch-buffer-size-changed-p))
      (message "**** ignore to save *scatch* buffer, buffer size not changed [%s], last changed time is [%s] ****" now *scratch-last-saved-time*))
     (t
      ;; mark buffer size
      (scratch-mark-buffer-size)
      ;; lock the status to saving
      (setq *scratch-autosave-status* 'saving)
      (with-current-buffer buffer
        (let ((content (buffer-substring-no-properties (point-min) (point-max))))
          (with-temp-file +default-scratch-file-name+
            (insert content))))
      ;; update saved time
      (setq *scratch-last-saved-time* now)
      ;; unlock the status
      (scratch-reset-autosave-status)
      (message "**** *scratch* buffer saved to file %s [%s], lasted saved time: [%s] ****" +default-scratch-file-name+ now last-saved-time)))))

(add-hook 'emacs-startup-hook
          #'(lambda ()
              ;; load content from file first
              (when (load-scratch-from-file t)
                  ;; add the save-scratch-buffer to 'kill-emacs-hook after load file success,
                  ;;or it will save empty content to file dangerously.
                  (add-hook 'kill-emacs-hook 'save-scratch-buffer)
                  ;; start a scheduler to save buffer to file
                  (start-scratch-autosave-scheduler))))

(provide 'scratch-config)
;;; scratch-config.el ends here
