;;; package --- commands.el
;;; Commentary:
;;; Code:


(defvar *original-key-bindings* nil
  "A list to store original key bindings for restoration.")

(defun command-or-anonymous-string (command)
  (if (and (symbolp command) (fboundp command))
      (symbol-name command) "<anonymous function>"))

(defun list-re-binding-keys ()
  (interactive)
  (let ((list))
    (dolist (binding *original-key-bindings*)
      (let* ((string-key (car binding))
             (vector-key (kbd string-key))
             (original-command (assoc-default string-key *original-key-bindings* #'string-equal))
             (original-command-str (command-or-anonymous-string original-command))
             (new-command (lookup-key (current-global-map) vector-key)))
        (push `(,(format "%s: %s -> %s" string-key original-command-str new-command) ,string-key ,original-command ,new-command) list)))
    (completing-read "The re-binding keys: " list nil t)))

(defun bind-command (command)
  "Bind COMMAND to a key and save the original binding."
  (interactive "CPlease select a command: ")
  (let* ((key (read-key "Press the key you want to bind: "))
         (vector-key (vector key))
         (string-key (key-description vector-key))
         (original-command))
    (unless (assoc-default string-key *original-key-bindings* #'string-equal)
      (setq original-command (lookup-key (current-global-map) vector-key))
      (setq *original-key-bindings* (cons (cons string-key original-command) *original-key-bindings*))
      (global-set-key vector-key command)
      (message "Bound %s to %s" string-key (symbol-name command)))))

(defun unbind-command ()
  "Unbind the last bound command and restore the original binding."
  (interactive)
  (when *original-key-bindings*
    (let* ((string-key (completing-read "Select a key binding: " *original-key-bindings* nil t))
           (original-command (assoc-default string-key *original-key-bindings* #'string-equal))
           (vector-key (kbd string-key)))
      (when original-command
        (global-set-key vector-key original-command)
        (setq *original-key-bindings* (cdr *original-key-bindings*))
        (message "Restored %s to %s" (key-description vector-key) (command-or-anonymous-string original-command))))))

(provide 'commands)
;;; commands.el ends here
