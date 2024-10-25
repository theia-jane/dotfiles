;;; package-utils.el -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:
(defun update-repo-load-path (&optional config-dir)
  (interactive)
  (message "root %s" config-dir)
  (let ((root (or config-dir (substitute-env-vars "$dot"))))
    (unless (string= "" root)
	(dolist (d (cons root
		     (seq-filter #'file-directory-p (directory-files-recursively root "" t))))
	  (add-to-list 'load-path (file-truename d))))))

(update-repo-load-path
  (file-name-concat (file-name-directory load-file-name) ".."))

(require 'variable-utils)
(require 'bootstrap-straight) 
(require 'process-helpers) 
(require 'info)
(require 'async)

(defconst global-packages--running-env-var "EMACS_GLOBAL_PACKAGES_RUNNING")
(defconst global-packages--running-p (equal (getenv global-packages--running-env-var) "TRUE"))
(defvar global-packages-dir
  (expand-file-name "~/.local/emacs/site-lisp/global-packages/"))

(defmacro global-packages--intern (&rest body) 
  `(progn 
     (setq user-emacs-directory ,global-packages-dir
           global-packages--in-session-p t)
     (require 'bootstrap-straight) 
     (require 'info) 
     (eject-differences ,global-packages-save-variables
                        (bootstrap-straight)
                        ,@body)))

(defconst global-packages-save-variables '(load-path Info-directory-list))

(defmacro global-packages (&rest body)
  (unless global-packages--running-p
    `(progn 
       ;; (write-region "Global packages called...\n" nil "/tmp/test-log" 'append)
       (setenv global-packages--running-env-var "TRUE")
       (let ((p (async-start
                 (lambda () (global-packages--intern ,@body))
                 #'add-all-to-lists)))
         (wait-for-process p)
         (setenv global-packages--running-env-var "FALSE")))))

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



(provide 'package-utils)
;;; package-utils.el ends here
