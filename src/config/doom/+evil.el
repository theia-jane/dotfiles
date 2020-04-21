;;; ~/Projects/dotfiles/src/config/doom/+evil.el -*- lexical-binding: t; -*-
;;;

;; TODO Make a PR to pull in the emulated eol stuff into evil
(defun +evil-is-emulated-eol? ()
  "Determines if the point's current position is an emulated eol."
  (and (or (evil-normal-state-p)
        (evil-operator-state-p))
       (= (1+ (point))
          (line-end-position))))

(defadvice! +evil--center-ex-search (&rest _)
  :after #'evil-ex-search
  (recenter nil t))

;; FIXME -- this messes up evil-up-paren
;;          1. Get debugging working
;;          2. Debug through the working function, which can be 'called' by
;;             doing =da(= on a paren on the begining of a line
;;          3. Debug through the broken version
(defadvice! +evil--forward-sexp--advice (fn &rest args)
  "Outside of the emulated eol with forward sexp"
  :around '(forward-sexp up-list)
  (when (+evil-is-emulated-eol?)
    (forward-char))
  (apply fn args))
