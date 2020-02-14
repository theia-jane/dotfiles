;;; ~/Projects/dotfiles/src/config/doom/+core.el -*- lexical-binding: t; -*-

(defun load!? (filename)
  "Check if the file exists in the (dir!) first and then (load!)"
  (if (file-exists-p! filename (dir!))
      (load! filename)))
