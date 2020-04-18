;;; ~/Projects/dotfiles/src/config/doom/+elisp.el -*- lexical-binding: t; -*-


(defun +elisp/goto-start-of-outer-sexp ()
  (interactive)
  (with-syntax-table emacs-lisp-mode-syntax-table
    (let ((next t))
      (while next
        (setq next nil)
        (ignore-error scan-error
          (when (and (eolp) (not (bolp)))
            (backward-char))
          (backward-up-list)
          (setq next t))))))

(defun +elisp/goto-end-of-outer-sexp ()
  (interactive)
  (with-syntax-table emacs-lisp-mode-syntax-table
    (let ((next t))
      (while next
        (setq next nil)
        (ignore-error scan-error
          (when (and (bolp) (not (eolp)))
            (forward-char))
          (up-list 1)
          (setq next t))))))


(defun +elisp/eval-outer-sexp (eval-last-sexp-arg-internal)
  "Evaluate outermost sexp, this utilizes the 'eros-eval-last-sexp"
  (interactive "P")
  (save-excursion
    (+elisp/goto-end-of-outer-sexp)
    (eros-eval-last-sexp eval-last-sexp-arg-internal)))


(defun +elisp/eval-outer-sexp-then-next (eval-last-sexp-arg-internal)
  "Evaluate outermost sexp, this utilizes the 'eros-eval-last-sexp"
  (interactive "P")
  (+elisp/goto-end-of-outer-sexp)
  (condition-case _err
      (eros-eval-last-sexp eval-last-sexp-arg-internal)
    (t
     (forward-sexp)
     (eros-eval-last-sexp eval-last-sexp-arg-internal)))
  (forward-sexp)
  (+elisp/goto-start-of-outer-sexp))


;; Key-bindings
(map!
 (:map ctl-x-map
   ;; Replace 'eros-eval-last-sexp
   :desc "Eval outermost sexp" "C-e" #'+elisp/eval-outer-sexp
   ;; Move 'eros-eval-last-sexp
   "C-S-e" #'eros-eval-last-sexp)
 (:map emacs-lisp-mode-map
   :desc "Eval outermost sexp" "<S-return>" #'+elisp/eval-outer-sexp
   "<C-S-return>" #'+elisp/eval-outer-sexp-then-next)
 (:localleader
   :map emacs-lisp-mode-map
   :desc "Eval outermost sexp" "e o" #'+elisp/eval-outer-sexp)
 )


