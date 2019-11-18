(dolist (spacemacs-file '(layers init user-init user-config))
  (load (format "%s.el" spacemacs-file)))
