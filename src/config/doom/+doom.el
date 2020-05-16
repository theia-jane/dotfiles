;;; ~/Projects/dotfiles/src/config/doom/+doom.el -*- lexical-binding: t; -*-


(setq doom-scratch-initial-major-mode 'org-mode
      doom-font (font-spec :family "Source Code Pro" :size 30)
      doom-variable-pitch-font (font-spec :family "Source Code Pro")
      doom-unicode-font (font-spec :family "DejaVu Sans Mono")
      doom-big-font (font-spec :family "Source Code Pro" :size 48))


(defun +doom-buffer-name (context &optional global &rest extra-context)
  "Generates a canonical doom buffer name.

With just a CONTEXT set you get: *doom:<context>*

When GLOBAL is nil the workspace name or main (the default) is appended to the name, like so: *doom:<context>:main*

Whatever EXTRA-CONTEXT is provided is appended after the CONTEXT: *doom:<context>:<extra>:<context>:main*
"
  (let ((scope-list `("doom"
                      ,context
                      ,@extra-context
                      ,(and (null global)
                            (if (bound-and-true-p persp-mode)
                                (safe-persp-name (get-current-persp))
                              "main")))))
    (concat "*" (string-join (seq-filter 'identity scope-list) ":") "*")))

