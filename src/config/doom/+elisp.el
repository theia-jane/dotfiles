;;; ~/Projects/dotfiles/src/config/doom/+elisp.el -*- lexical-binding: t; -*-

(defun +elisp/eval-outer-sexp ()
  (interactive)
  (save-excursion
    (let* ((ppss (syntax-ppss))
           (outer-sexp-posns (nth 9 ppss)))
      (when outer-sexp-posns
        (goto-char (car outer-sexp-posns))))
    (forward-char)
    (sp-end-of-sexp)
    (eros-eval-last-sexp nil)))


(general-define-key
  :keymaps 'ctl-x-map
  "C-e" '+elisp/eval-outer-sexp)

(general-define-key
  :keymaps 'emacs-lisp-mode-map
  "<S-return>" '+elisp/eval-outer-sexp)
