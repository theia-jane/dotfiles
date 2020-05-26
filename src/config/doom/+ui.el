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
        evil-respect-visual-line-mode t
        line-spacing 14
        fill-column 100)
  (auto-fill-mode 1)
  (if (> (line-number-at-pos (point-max)) 1500)
    (org-overview)) ;; Is there an earlier point I can set this up at?
  (vi-tilde-fringe-mode -1)
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
