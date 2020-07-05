;;; ~/Projects/dotfiles/src/config/doom/+elisp.el -*- lexical-binding: t; -*-

(defun beginning-of-sexp-p ()
  "Test if the point is at the beginning of a sexp."
  (= (point)
     (save-excursion
       (end-of-sexp)
       (beginning-of-sexp)
       (point))))

(defun end-of-sexp-p ()
  "Test if the point is at the end of a sexp."
  (= (point)
     (save-excursion
       (beginning-of-sexp)
       (end-of-sexp)
       (point))))

(defun outermost-list (&optional escape-strings no-syntax-crossing)
  "Move point to the end of the outermost list relative to the current point."
  (interactive "d\nd")
  ;; Don't ignore scan-error if we start at the wrong spot
  (up-list 1 escape-strings no-syntax-crossing)
  (ignore-error scan-error
    (cl-loop (up-list 1 escape-strings no-syntax-crossing))))

(defun end-of-outermost-sexp ()
  (interactive)
  (ignore-error scan-error
    (outermost-list t nil))
  (if (beginning-of-sexp-p)
      (end-of-sexp)))

(defun +elisp/eval-sexp-under-point (eval-last-sexp-arg-internal)
  (interactive "P")
  (with-syntax-table emacs-lisp-mode-syntax-table
    (save-excursion
      (end-of-sexp)
      (eros-eval-last-sexp eval-last-sexp-arg-internal))))

(defun +elisp/eval-outermost-sexp (eval-last-sexp-arg-internal)
  "Evaluate outermost sexp, this utilizes the 'eros-eval-last-sexp"
  (interactive "P")
  (with-syntax-table emacs-lisp-mode-syntax-table
    (save-excursion
      (end-of-outermost-sexp)
      (eros-eval-last-sexp eval-last-sexp-arg-internal))))


(defun +elisp/eval-outermost-sexp-and-continue (eval-last-sexp-arg-internal)
  "Attempts to evaluate outermost sexp. If it fails, move to next sexp.
This utilizes the 'eros-eval-last-sexp."
  (interactive "P")
  (with-syntax-table emacs-lisp-mode-syntax-table
    (ignore-error end-of-file
      (+elisp/eval-outermost-sexp eval-last-sexp-arg-internal))
    (end-of-outermost-sexp)
    (forward-sexp)
    (beginning-of-sexp))
  (recenter nil t))


(defadvice! +elisp/correct-evil-emulate-eol (fn &rest args)
  "Outside of the emulated eol with forward sexp"
  :around '(beginning-of-sexp-p
            end-of-sexp-p
            outermost-list
            end-of-outermost-sexp
            +elisp/eval-outermost-sexp
            +elisp/eval-outermost-sexp-and-continue)
  (let ((+evil--should-correct-for-eol t))
    (apply fn args)))


;; Key-bindings
(map!
 (:map ctl-x-map
   ;; Replace 'eros-eval-last-sexp
   :desc "Eval outermost sexp" "C-e" #'+elisp/eval-outer-sexp
   ;; Move 'eros-eval-last-sexp
   "C-S-e" #'eros-eval-last-sexp)
 (:map emacs-lisp-mode-map
   :desc "Eval outermost sexp" "<S-return>" #'+elisp/eval-outermost-sexp
   "<C-return>" #'+elisp/eval-outermost-sexp-and-continue)
 (:localleader
   :map emacs-lisp-mode-map
   :desc "Eval outermost sexp" "e o" #'+elisp/eval-outer-sexp)
 )


