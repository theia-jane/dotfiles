;;; variable-utils.el -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:
(require 'seq)

(defmacro save-variables (subject &rest body)
  (when (symbolp subject)
    (setq subject (list subject)))
  `(let (,@(mapcar
            (lambda (symb) `(,symb ,symb))
            subject))
     ,@body))

(defmacro seq-differences-after (subject &rest body)
  (when (symbolp subject)
    (setq subject (list subject)))
  (let ((subject-pairs
         (mapcar
          (lambda (symb)
            `(,(intern (concat (symbol-name symb) "-before"))
              ,symb))
          subject)))

    `(let (,@subject-pairs)
       ,@body
       (list ,@(mapcar
                (lambda (pair)
                  (let ((a (car pair))
                        (b (cadr pair)))
                    `(cons ',b (seq-difference ,b ,a))))
                subject-pairs)))))

(defmacro eject-differences (subject &rest body)
  `(save-variables
    ,subject
    (seq-differences-after
     ,subject
     ,@body)))

(defun add-all-to-list (var var-values)
  (dolist (value var-values)
    (add-to-list var value)))

(defun add-all-to-lists (var-forms)
  (dolist (var-form var-forms)
    (add-all-to-list (car var-form) (cadr var-form))))


(defun unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list.

Note: excludes keymaps as `lists'"
  (declare (pure t) (side-effect-free t))
  (if (and (listp exp)
           (not (keymapp exp)))
      exp
    (list exp)))

(defun keyword-intern (str)
  "Converts STR (a string) into a keyword (`keywordp')."
  (declare (pure t) (side-effect-free t))
  (cl-check-type str string)
  (intern (concat ":" str)))

(defun keyword-name (keyword)
  "Returns the string name of KEYWORD (`keywordp') minus the leading colon."
  (declare (pure t) (side-effect-free t))
  (cl-check-type keyword keyword)
  (substring (symbol-name keyword) 1))


(provide 'variable-utils)
;;; variable-utils.el ends here
