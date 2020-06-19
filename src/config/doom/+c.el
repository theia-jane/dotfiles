;;; ~/Projects/dotfiles/src/config/doom/+c.el -*- lexical-binding: t; -*-

;; Make sure these get loaded
;; UPSTREAM
(use-package! ob-C
  :commands (org-babel-execute:C
             org-babel-execute:C++
             org-babel-execute:D))
