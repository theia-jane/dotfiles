;;; ~/Projects/dotfiles/src/config/doom/+doom.el -*- lexical-binding: t; -*-


(setq doom-scratch-initial-major-mode 'org-mode)
(setq doom-font (font-spec :family "Source Code Pro" :size 30)
      doom-variable-pitch-font (font-spec :family "Source Code Pro")
      doom-unicode-font (font-spec :family "DejaVu Sans Mono")
      doom-big-font (font-spec :family "Source Code Pro" :size 48))

(defun +doom-buffer-name (context)
  (format "*doom:%s:%s*"
          context
          (if (bound-and-true-p persp-mode)
              (safe-persp-name (get-current-persp))
            "main")))
