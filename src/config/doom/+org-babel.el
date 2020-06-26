;;; ~/Projects/dotfiles/src/config/doom/+org-babel.el -*- lexical-binding: t; -*-


(defun define-ob--make-symbol (fmt sym)
  (intern (format fmt (symbol-name sym))))

;; Making an org babel definer (cause why not?)
(cl-defmacro define-ob! (name &key
                              execute
                              assign-variables
                              expand-body
                              prep-edit
                              prep-session
                              header-args
                              lang
                              file-ext)
  "TODO"
  (unless execute
    (error "Must at least provide a babel execute function."))

  `(let ((header-args ,header-args)
         (lang ,lang)
         (file-ext ,file-ext))

     (defun ,(define-ob--make-symbol "org-babel-execute:%s" name)
         (body params)
       (funcall ,execute body params))

     (when header-args
       (defconst ,(define-ob--make-symbol "org-babel-header-args:%s" name) header-args ""))

     (when lang
       (add-to-list 'org-src-lang-modes (cons (symbol-name ',name) lang)))

     (when file-ext
       (add-to-list 'org-babel-tangle-lang-exts (cons (symbol-name ',name) file-ext)))))

;; (define-ob! guile
;;   :execute (lambda (body params)
;;              (message "%s %s" body params))
;;   :lang "scheme")
