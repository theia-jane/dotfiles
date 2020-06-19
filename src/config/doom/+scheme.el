;;; ~/Projects/dotfiles/src/config/doom/+scheme.el -*- lexical-binding: t; -*-

(after! geiser
  (defvar +geiser-silent-start nil)

  ;; Setting a default shuts up geiser...
  (setq geiser-default-implementation 'guile)

  (defadvice! +geiser--ob-scheme (fn &rest args)
    "Force geiser to run in the background."
    :around #'org-babel-scheme-execute-with-geiser
    (let ((+geiser-silent-start t))
      (apply fn args)))

  (defadvice! +geiser--quiet-repl-start (fn &rest args)
    "Enable geiser to open a repl in the background (silently)."
    :around #'geiser-repl--start-repl
    (if +geiser-silent-start
        (let ((silent-fn (lambda (buf) (set-buffer buf))))
          (cl-letf (((symbol-function 'switch-to-buffer) silent-fn)
                    ((symbol-function 'switch-to-buffer-other-window) silent-fn))
            (apply fn args)))
      (apply fn args)))

  (defadvice! +org-src--edit-save-off-geiser (args)
    "Seting geiser scheme implemenation to be src blocks impl."
    :filter-args #'org-src--edit-element
    (when (eq 'scheme-mode (nth 2 args))
      (when-let* ((babel-info (org-babel-get-src-block-info 'light))
                  (scheme-param (assoc :scheme (nth 2 babel-info)))
                  (scheme-impl (cdr scheme-param)))
        (setf (nth 2 args) `(lambda ()
                              (setq-local geiser-scheme-implementation ,scheme-impl)
                              (funcall #',(nth 2 args))))))
    args))
