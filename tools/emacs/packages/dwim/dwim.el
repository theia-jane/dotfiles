;;; dwim.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 
;;
;; Author:  <https://github.com/Symbol’s function definition is void: doom-call-process>
;; Maintainer:  <tware@tware-arch>
;; Created: August 19, 2021
;; Modified: August 19, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/tware/dwim
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  
;;
;;; Code:


(defun dwim--handle-case (val)
  "Take VAL and if it's a command or function execute it.

Otherwise we return the value."
    (cond ((commandp val) (command-execute val))
          ((functionp val) (funcall val))
            (t val)))

(defmacro def-dwim (name &rest options)
  "Define a DWIM function with NAME-dwim and OPTIONS.

OPTIONS may be:
- :interactive value 
  value is optional. If this key is present then the functional will
  be interactive

- :applies-when pred
  pred is required. Function will only do something when pred is true

- :default value
  value is required. When present it sets the default return value. If
  a symbol is provided that's a function, then the function is called

- :default-return value
  value is required. When present it sets a default return value to return.

- :case pred value
  pred and value are required. When present it creates a new case,
  when the pred evaluates to true the value is handled. Either it is
  called if it's a symbol with a function value or it's evaluated.
  
  The first case wins if there are conflicting cases that match

- :case-return pred value
  pred and value are required. When present it creates a new case,
  when the pred evaluates to true the value is evaluated and returned. 
  
  The first case wins if there are conflicting cases that match
"
  (let (let-values
        applies-when
        cases
        default-case
        interactive-p
        interactive-args)
    (while options
      (let ((option (pop options)))
       (pcase option
          ;; Should resulting function be interactive?
          (:interactive
           (setq interactive-p t)
           (when (and (car options) (listp (car options)))
             (setq interactive-args (pop options))))

          (:let
            (when (and (not let-values) (listp (car options)))
              (setq let-values (append let-values (pop options)))))

          ;; First default value wins
          (:default 
           (when (and (not default-case) (car options))
             (setq default-case `(t (dwim--handle-case ,(pop options))))))
          (:default-return
           (when (and (not default-case) (car options))
             (setq default-case `(t ,(pop options)))))

          (:case
           (when (>= (length options) 2)
             (setq cases (cons `(,(pop options) (dwim--handle-case ,(pop options))) cases))))
          (:case-return
           (when (>= (length options) 2)
             (setq cases (cons `(,(pop options) ,(pop options)) cases))))

          (:applies-when
           (when (and (not default-case) (car options))
             (setq applies-when (cons (pop options) applies-when)))))))

    `(defun ,(intern (concat (symbol-name name) "-dwim")) ,(when interactive-args '(&optional arg))
       ,(when interactive-p (cons 'interactive interactive-args))
       (when (or ,(unless applies-when t) ,@(reverse applies-when))
         (let* ,let-values
           (cond 
             ,@(reverse cases) ;; put cases in reverse order 
             ,default-case))))))

(defun dwim-undefined ()
    (message "No defined action."))

(provide 'dwim)
;;; dwim.el ends here
