;;; ~/Projects/dotfiles/src/config/doom/+elisp.el -*- lexical-binding: t; -*-

(defun +elisp/eval-outer-sexp (eval-last-sexp-arg-internal)
  "Evaluate outermost sexp, this utilizes the 'eros-eval-last-sexp"
  (interactive "P")
  (save-excursion
    (let* ((ppss (syntax-ppss))
           (outer-sexp-posns (nth 9 ppss)))
      (when outer-sexp-posns
        (goto-char (car outer-sexp-posns))))
    (if (not (eq nil (sexp-at-point)))
        (progn (sp-end-of-next-sexp)
               (eros-eval-last-sexp eval-last-sexp-arg-internal))
      (message "Not inside sexp"))))


(general-define-key
  :keymaps 'ctl-x-map
  "C-e" '+elisp/eval-outer-sexp)

(general-define-key
  :keymaps 'emacs-lisp-mode-map
  "<S-return>" '+elisp/eval-outer-sexp)
