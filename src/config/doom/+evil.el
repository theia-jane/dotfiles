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

(defadvice! +evil--forward-sexp--advice (fn &rest args)
  "Outside of the emulated eol with forward sexp"
  :around 'forward-sexp
  (when (+evil-is-emulated-eol?)
    (forward-char))
  (apply fn args))
