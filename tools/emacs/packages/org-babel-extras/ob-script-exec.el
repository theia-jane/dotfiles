;;; ob-script-exec.el -*- lexical-binding: t; -*-

(defvar org-babel-eval-script-fn #'ob-eval-script-async-shell)

(defun ob-eval-script-async-shell (command)
  (let ((buff (generate-new-buffer
               (concat "*org-babel:eval:" (buffer-name) "*"))))
    
    (async-shell-command command buff)
  (with-current-buffer buff
    (setq header-line-format command))))

(defun org-babel-src-get-dir (header-args)
  (let ((dir (cdr (assq :dir header-args))))
    (if (and dir (not (file-remote-p dir)))
        dir
      default-directory)))

(defadvice! ob-src-execute-subprocess-a (fn &optional arg info params)
  :around 'org-babel-execute-src-block
  (let* ((info (or info (org-babel-get-src-block-info)))
         (property-list (nth 2 (org-babel-get-src-block-info)))
         (subprocess-p (not (eq nil
                                (seq-find (lambda (x) (eq (car x) :subprocess))
                                          property-list)))))
    (if subprocess-p
        (org-babel-execute-src-block-as-subprocess)
      (funcall fn arg info params))))

(defun org-babel-execute-src-block-as-subprocess (&rest _)
  "TODO"
  (interactive)
  (cl-destructuring-bind
      (lang contents header-args &rest _)
      (org-babel-get-src-block-info)
    (cond ((member lang org-babel-shell-names)
           (if (not (equal lang "eshell"))
               (org-babel-src-evaluate-as-script
                lang
                (org-babel-expand-body:generic
                 contents
                 header-args
                 (org-babel-variable-assignments:shell header-args))
                header-args))))))

(defun org-babel-src-evaluate-as-script (lang contents header-args)
  "TODO"
  (let* ((dir (org-babel-src-get-dir header-args))
         (stdin (let ((stdin (cdr (assq :stdin header-args)))
                      (inhibit-message t))
                  (when stdin
                    (org-babel-sh-var-to-string (org-babel-ref-resolve stdin)))))
         (cmdline (cdr (assq :cmdline header-args)))
         (shebang (or
                   (cdr (assq :shebang header-args))
                   (format "#!/usr/bin/env %s" lang)))
         (script-file (org-babel-temp-file (concat lang "-script-")))
         (stdin-file (org-babel-temp-file "stdin-"))
         (padline (not (string= "no" (cdr (assq :padline header-args))))))
    (with-temp-file script-file
      (insert shebang "\n")
      (when padline (insert "\n"))
      (insert contents))
    (set-file-modes script-file #o755)
    (let ((command-string
           (cond
            ((or stdin cmdline)
             (with-temp-file stdin-file (insert (or stdin "")))
             (concat "cat " stdin-file " | " script-file (and cmdline (concat " " cmdline))))
            (t script-file)))
          (default-directory dir))
      (funcall org-babel-eval-script-fn command-string))))

(provide 'ob-script-exec)
;;; ob-script-exec.el ends here
