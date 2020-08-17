;;; ~/Projects/dotfiles/src/config/doom/+packages.el -*- lexical-binding: t; -*-


(use-package! all-the-icons-ivy
  :after ivy
  :defer-incrementally t
  :config
  (all-the-icons-ivy-setup))

(use-package! all-the-icons-dired
  :after dired
  :defer-incrementally t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package! try
  :defer-incrementally t)
