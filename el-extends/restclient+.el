;;; package --- restclient+.el
;;; Commentary:
;;; Code:

(require-if-installed
 'restclient
 (defun restclient-find-vars-before-point ()
   (let ((vars nil)
         (bound (point)))
     (save-excursion
       (goto-char (point-min))
       (while (search-forward-regexp restclient-var-regexp bound t)
         (let* ((name (match-string-no-properties 1))
                (ename (concat "rc--" (substring name 1)))
                (should-eval (> (length (match-string 2)) 0))
                (value (or (restclient-chop (match-string-no-properties 4)) (match-string-no-properties 3)))
                (value (if should-eval (restclient-eval-var value) value)))
           (setq vars (cons (cons name value) vars))
           (setq value (restclient-replace-all-in-string vars value))
           (let ((symbol (intern ename)))
             (set symbol value))))
       vars)))

 (defun restclient-parse-from-alist (json-object key)
   (let* ((pair (assoc key json-object)))
     (cdr pair)))

 (defvar restclient-response-loaded-hook nil)
 (defvar rc--current-token nil)
 (defvar rc--verify-code nil)
 (defvar rc--uap-token nil)
 
 (setq restclient-response-loaded-hook
       (lambda ()
         (message "restclient-response-received-hook")
         (let* ((result-object (json-read))
                (access-token (restclient-parse-from-alist result-object 'accessToken))
                (verify-code (restclient-parse-from-alist result-object 'verifycode))
                (token (restclient-parse-from-alist result-object 'token)))
           (when access-token
             (setq rc--current-token access-token)
             (message "access-token: %S" access-token))
           (when verify-code
             (setq rc--verify-code verify-code)
             (message "verify-code: %S" verify-code))
           (when token
             (setq rc--uap-token token)
             (message "token: %S" token))))))



(provide 'restclient+)
;;; restclient+.el ends here