;;; ~/Projects/dotfiles/src/config/doom/+ui.el -*- lexical-binding: t; -*-

(defun +ui/scroll-to-percent (percent)
  (interactive)
  (recenter
   (truncate (* (window-body-height) percent))
   t))

(defmacro set-face-attributes! (&rest attributes-list)
  `(seq-do #'(lambda (attributes)
            (apply #'set-face-attribute (car attributes) nil (cdr attributes)))
          ',attributes-list))

(after! org
  (set-face-attributes!
   (org-document-title :height 1.5)
   (org-level-1 :height 1.1)
   (org-level-2 :height 1.05)
   (org-level-3 :height 1.025)
   (org-document-info-keyword :height 1.0)))

(after! dired
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

(load-theme 'doom-gruvbox t)

(setq-default line-spacing 7)
