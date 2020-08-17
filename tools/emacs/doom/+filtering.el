;;; ../../Projects/dotfiles/src/config/doom/+filtering.el -*- lexical-binding: t; -*-

(use-package! dired-narrow
  :commands (dired-narrow))

(map! :map dired-mode-map
      :no "g /" #'dired-narrow)
