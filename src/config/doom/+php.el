;;; ~/Projects/dotfiles/src/config/doom/+php.el -*- lexical-binding: t; -*-


(after! lsp-mode
  (add-hook #'php-mode-hook #'lsp-headerline-breadcrumb-mode))
