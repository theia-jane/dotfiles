;;; .doom.d/config.el -*- lexical-binding: t; -*-



;;; NOTE: (use-package!) should be used with care and specifically with defering keywords
;;;       Using use-package! without a deferring keyword (one of: :defer :after
;;;       :commands :defer-incrementally :after-call) will load the package
;;;       immediately. This can cause other packages to be pulled in and loaded,
;;;       which will compromise many of Doom’s startup optimizations.
(use-package! all-the-icons-ivy
  :config
  (all-the-icons-ivy-setup))

(use-package! all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package! i3wm)
