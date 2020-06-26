;;; ~/Projects/dotfiles/src/config/doom/+dsl.el -*- lexical-binding: t; -*-

;; TODO: DSL Definer
(defmacro define-dsl (name &rest definition)
  (cl-flet ((new-symb (fmt symb)
                      (intern (format fmt (symbol-name symb)))))

  `(progn
     (defun ,name (&rest exps)
       forms)

     (defun ,(new-symb "%s--expand-exp") (exp)
       (pcase exp
         ((pred ))

         ))

     (defun ,(new-symb "%s-define" symb)) (name &rest args)
       (cons name forms))))

