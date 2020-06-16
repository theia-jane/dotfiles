;;; ~/Projects/dotfiles/src/config/doom/+hacks.el -*- lexical-binding: t; -*-


;; Make sure that the elc advice isn't an issue by redefining it here
(after! org
  (defun org--newline (indent arg interactive)
    "Call `newline-and-indent' or just `newline'.
If INDENT is non-nil, call `newline-and-indent' with ARG to
indent unconditionally; otherwise, call `newline' with ARG and
INTERACTIVE, which can trigger indentation if
`electric-indent-mode' is enabled."
    (if indent
        (org-newline-and-indent arg)
      (newline arg interactive))))
