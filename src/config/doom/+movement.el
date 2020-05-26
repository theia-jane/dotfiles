;;; ~/Projects/dotfiles/src/config/doom/+movement.el -*- lexical-binding: t; -*-

(defadvice! +movement--recenter-afterward (&rest _)
  :after '(evil-ex-search
           counsel-outline-action)
  (recenter nil t))
