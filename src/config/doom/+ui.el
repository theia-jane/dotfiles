;;; ~/Projects/dotfiles/src/config/doom/+ui.el -*- lexical-binding: t; -*-

(defun +ui/scroll-to-percent (percent)
  (interactive)
  (recenter
   (truncate (* (window-body-height) percent))
   t))

(after! dired
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

(setq-default line-spacing 7)
