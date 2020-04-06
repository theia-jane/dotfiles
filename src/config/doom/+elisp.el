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


;; Key-bindings
(map!
 (:map ctl-x-map
   ;; Replace 'eros-eval-last-sexp
   :desc "Eval outermost sexp" "C-e" #'+elisp/eval-outer-sexp
   ;; Move 'eros-eval-last-sexp
   "C-S-e" #'eros-eval-last-sexp)
 (:map emacs-lisp-mode-map
   :desc "Eval outermost sexp" "<S-return>" #'+elisp/eval-outer-sexp)
 (:localleader
   :map emacs-lisp-mode-map
   :desc "Eval outermost sexp" "e o" #'+elisp/eval-outer-sexp)
 )
