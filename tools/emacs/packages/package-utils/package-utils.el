;;; package-utils.el -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:
(defun update-repo-load-path (&optional config-dir)
  (interactive)
  (let ((root (or config-dir (substitute-env-vars "$dot"))))
    (unless (string= "" root)
	(dolist (d (cons root
		     (seq-filter #'file-directory-p (directory-files-recursively root "" t))))
	  (add-to-list 'load-path (file-truename d))))))

(update-repo-load-path
  (file-name-concat (file-name-directory load-file-name) ".."))

(require 'bootstrap-straight)
(require 'variable-utils)

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
