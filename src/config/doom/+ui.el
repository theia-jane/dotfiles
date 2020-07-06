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


(defun +ui/non-code--display-hook()
  (setq display-line-numbers nil
        left-margin-width 5
        right-margin-width 5
        header-line-format " "
        evil-respect-visual-line-mode nil
        line-spacing 14
        fill-column 80)
  (if (> (line-number-at-pos (point-max)) 1500)
      (pcase major-mode
        ;; Is there an earlier point I can set this up at? Seems like it isn't as effective as #+STARTUP: overview
        (org-mode (org-overview))))

  ;; If margins are quite behaving like you want, you should take a look at what
  ;; visual-fill-column-mode is doing under the hood
  ;; (setq visual-fill-column-width 80
  ;;       visual-fill-column-center-text t)
  ;; (visual-fill-column-mode)
  (vi-tilde-fringe-mode -1)
  (hl-line-mode -1)

  )

(setq +ui/non-code--hook-list '(org-mode-hook
                                markdown-mode-hook))

(dolist (hook +ui/non-code--hook-list)
  (add-hook hook #'+ui/non-code--display-hook))

(add-hook 'writeroom-mode-disable-hook (lambda ()
                                         (if (memq major-mode +ui/non-code--hook-list)
                                             (mapc (lambda (w)
                                                     (with-selected-window w
                                                       (set-window-margins (selected-window) 5 5)))
                                                   (get-buffer-window-list (current-buffer) nil)))))


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

(defun dashboard-logo-header ()
(let ((point (point)))
  (insert (make-string 19 ?\n))
    (when (display-graphic-p)
      (let ((image (svg/as-image (doom/wayward-arcanist-logo :size "300"))))
        (add-text-properties
         point (point) `(display ,image rear-nonsticky (display)))
        (save-excursion
          (goto-char point)
          (insert (make-string
                   (truncate
                    (max 0 (+ 1 (/ (- +doom-dashboard--width
                                      (car (image-size image nil)))
                                   2))))
                   ? ))))
      (insert "\n"))))

(setq +doom-dashboard-functions
      '(dashboard-logo-header
        doom-dashboard-widget-shortmenu
        doom-dashboard-widget-loaded
        doom-dashboard-widget-footer))
