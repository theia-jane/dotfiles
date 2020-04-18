;;; ~/Projects/dotfiles/src/config/doom/+evil.el -*- lexical-binding: t; -*-

(defadvice! +evil--center-ex-search (&rest _)
  :after #'evil-ex-search
  (recenter nil t))
