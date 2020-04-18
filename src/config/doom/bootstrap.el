;;; ~/Projects/dotfiles/src/config/doom/bootstrap.el -*- lexical-binding: t; -*-

(defun load!? (filename)
  (cond ((file-exists-p! filename (dir!))
         (load! filename))
        ((file-exists-p! (concat filename ".el") (dir!))
         (load! (concat filename ".el")))
        ((file-exists-p! (expand-file-name filename))
         (load filename))))

(defun +load!? (&rest files)
  "Check if the file exists in the (dir!) first and then (load!)"
  (mapcar 'load!? files))
