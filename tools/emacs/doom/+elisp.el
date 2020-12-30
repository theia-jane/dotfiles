;;; ~/Projects/dotfiles/src/config/doom/+elisp.el -*- lexical-binding: t; -*-
(use-package my-elisp-extras)

(defadvice! +elisp/correct-evil-emulate-eol (fn &rest args)
  "Outside of the emulated eol with forward sexp"
  :around '(beginning-of-sexp-p
            end-of-sexp-p
            outermost-list
            end-of-outermost-sexp
            eval-outermost-sexp
            eval-outermost-sexp-and-continue)
  (let ((+evil--should-correct-for-eol t))
    (apply fn args)))

;; Key-bindings
(map!
 (:map ctl-x-map
   ;; Replace 'eros-eval-last-sexp
   :desc "Eval outermost sexp" "C-e" #'eval-outer-sexp
   ;; Move 'eros-eval-last-sexp
   "C-S-e" #'eros-eval-last-sexp)
 (:map emacs-lisp-mode-map
   :desc "Eval outermost sexp" "<S-return>" #'eval-outermost-sexp
   "<C-return>" #'eval-outermost-sexp-and-continue)
 (:localleader
   :map emacs-lisp-mode-map
   :desc "Eval outermost sexp" "e o" #'eval-outer-sexp))
