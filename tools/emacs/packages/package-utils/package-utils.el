;;; package-utils.el -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(require 'variable-utils)
(require 'bootstrap-straight) 
(require 'process-helpers) 
(require 'info)

(defvar global-packages-dir
  (expand-file-name "~/.local/emacs/site-lisp/global-packages/"))

(defun bootstrap-global-packages ()
  (message 
   (shell-command-to-string
    (format "emacs -Q --batch --eval %s"
            (shell-quote-argument
             (prin1-to-string
              (macroexpand-all
               '(global-packages--intern (straight-use-package 'async))))))))
  (unless (global-packages--load-async)
    (error "Failed to load async")))


(defun global-packages--load-async ()
  (or (require 'async nil t)
      (let ((path (concat (file-name-as-directory global-packages-dir)
                          "straight/build/async")))
        (when (locate-library "async" nil (list path))
          (add-to-list 'load-path path)
          (require 'async nil t)))))

(defmacro global-packages--intern (&rest body) 
  `(progn 
     (setq user-emacs-directory ,global-packages-dir)
     (require 'bootstrap-straight) 
     (require 'info) 
     (eject-differences ,global-packages-save-variables
                        (bootstrap-straight)
                        ,@body)))

(defconst global-packages-save-variables '(load-path Info-directory-list))

(defmacro global-packages (&rest body)
  (unless (global-packages--load-async)
    (bootstrap-global-packages))
  `(let ((p (async-start
    (lambda () (global-packages--intern ,@body))
    #'add-all-to-lists)))
     (wait-for-process p)))

(defmacro global-packages! (&rest recipes)
  `(global-packages
    ,@(mapcar
       (lambda (recipe)
         `(straight-use-package ',recipe))
       recipes)))

(defmacro require! (&rest requires)
  `(progn
     ,@(mapcar
        (lambda (rargs) `(apply #'require ',(enlist rargs)))
        requires)))

(defmacro packages! (&rest recipes)
  `(progn
     (bootstrap-straight)
    ,@(mapcar
       (lambda (recipe)
         `(straight-use-package ',recipe))
       recipes)))

(defun update-repo-load-path (&optional config-dir)
  (interactive)
  (add-all-to-list 'load-path
                   (seq-filter
                    #'file-directory-p
                    (directory-files-recursively
                     (concat 
                      (or config-dir (substitute-env-vars "$dot"))
                      "/tools/emacs/packages")
                     ""
                     t))))



(provide 'package-utils)
;;; package-utils.el ends here
