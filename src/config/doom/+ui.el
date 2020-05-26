;;; ~/Projects/dotfiles/src/config/doom/+ui.el -*- lexical-binding: t; -*-

;;; General appearance
(setq-default line-spacing 7)
(setq doom-font (font-spec :family "Source Code Pro" :size 30)
      doom-variable-pitch-font (font-spec :family "Source Code Pro" :size 30)
      doom-unicode-font (font-spec :family "DejaVu Sans Mono" :size 30)
      doom-big-font (font-spec :family "Source Code Pro" :size 48))

(load-theme 'doom-gruvbox t)

;;; Mode specific
(after! org
  (set-face-attributes!
   (org-document-title :height 1.5)
   (org-level-1 :height 1.1)
   (org-level-2 :height 1.05)
   (org-level-3 :height 1.025)
   (org-document-info-keyword :height 1.0)))

(defun +ui/org--display-hook()
  (setq display-line-numbers nil
        left-margin-width 5
        right-margin-width 5
        header-line-format " "
        line-spacing 14)
  (hl-line-mode -1))

(add-hook 'org-mode-hook #'+ui/org--display-hook)

(after! dired
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

;;; Commands
(defun +ui/scroll-to-percent (percent)
  (interactive)
  (recenter
   (truncate (* (window-body-height) percent))
   t))

;;; Helpers
(defmacro set-face-attributes! (&rest attributes-list)
  `(seq-do #'(lambda (attributes)
            (apply #'set-face-attribute (car attributes) nil (cdr attributes)))
          ',attributes-list))
