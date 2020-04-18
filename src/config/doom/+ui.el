;;; ~/Projects/dotfiles/src/config/doom/+ui.el -*- lexical-binding: t; -*-

(defun +ui/scroll-to-percent (percent)
  (interactive)
  (recenter
   (truncate (* (window-body-height) percent))
   t))

(after! dired
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

;; General Visual
(defun config/visual/buffer-spacing ()
  (setq-default
   line-spacing 7
   ;; header-line-format " "                                ; Add content to header line
   left-margin-width 3
   right-margin-width 3
   )
  ;; (set-face-attribute 'header-line nil :background "15") ; Set the background color of the header line to the background of the theme
  (set-window-margins nil 2 2)                           ; Add padding at the sides
 )

(config/visual/buffer-spacing)
