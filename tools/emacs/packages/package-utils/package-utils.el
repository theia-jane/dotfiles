;;; package-utils.el -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(require 'variable-utils)
(require 'bootstrap-straight) 
(require 'process-helpers) 
(require 'info)
(require 'async (file-name-concat (file-name-directory load-file-name) "../async/async.el"))

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
