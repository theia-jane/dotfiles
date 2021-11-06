;;; commands-lib.el  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  
;;
;;; Code:

(defmacro run-commands (result-fn &rest commands)
    `(with-temp-buffer
      (let ((after-change-functions after-change-functions))
        (add-to-list 'after-change-functions
                     #'(lambda (start end _)
                         (funcall ,result-fn (buffer-substring start
                                                    ;; chop off trailing newline
                                                    (1- end)))))
        (dolist (command ',commands)
                (shell-command command (current-buffer))))))

(defun d (command)
  (shell-command (concat "d " command)))


(defun firefox (url &optional container)
  (let ((url (if container
                 (format "ext+container:name=%s&url=%s"
                         container
                         (url-hexify-string url))
                 url)))
    (when url
      (d (format "firefox '%s'" url)))))


(provide 'commands-lib)
;;; commands-lib.el ends here
