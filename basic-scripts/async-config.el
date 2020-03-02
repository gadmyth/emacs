;;; package --- async-config.el
;;; Commentary:
;;; Code:

(require 'async)

(defmacro call-after (time &rest body)
  "Evaluate BODY after TIME seconds."
  `(async-start
    (lambda () (sleep-for ,time) ,time)
    (lambda (arg) ,@body)))

(provide 'async-config)
;;; async-config.el ends here
