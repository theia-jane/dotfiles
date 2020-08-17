;;; ~/Projects/dotfiles/src/config/doom/+pretty-stuff.el -*- lexical-binding: t; -*-


(defvar +font-lock-icons-default-prefix-format "\\(%s\\)" "TODO")

(defvar +font-lock-icons-default-icon-resolver #'identity
  "TODO")

(defun +prettify--compose (symbol)
  "Compose a sequence of characters into a symbol.
Regexp match data 0 specifies the characters to be composed."
  ;; Check that the chars should really be composed into a symbol.
  (let ((start (match-beginning 1))
        (end (match-end 1))
        (match (match-string 1)))
    (if (and (not (equal prettify-symbols--current-symbol-bounds (list start end)))
             (funcall prettify-symbols-compose-predicate start end match))
        (with-silent-modifications
          (when (= 1 (length symbol))
            (compose-region start end symbol))

          (add-text-properties
           start end
           `(prettify-symbols-start ,start
                                    prettify-symbols-end ,end
                                    display ,symbol)))
      ;; No composition for you.  Let's actually remove any
      ;; composition we may have added earlier and which is now
      ;; incorrect.
      (remove-list-of-text-properties start end
                                      '(display
                                        composition
                                        prettify-symbols-start
                                        prettify-symbols-end))))
  ;; Return nil because we're not adding any face property.
  nil)

(defadvice! +prettify-symbols--extended--post-command-hook ()
  ""
  :override 'prettify-symbols--post-command-hook
  (cl-labels ((get-prop-as-list
               (prop)
               (remove nil
                       (list (get-text-property (point) prop)
                             (when (and (eq prettify-symbols-unprettify-at-point 'right-edge)
                                        (not (bobp)))
                               (get-text-property (1- (point)) prop))))))
    ;; Re-apply prettification to the previous symbol.
    (when (and prettify-symbols--current-symbol-bounds
               (or (< (point) (car prettify-symbols--current-symbol-bounds))
                   (> (point) (cadr prettify-symbols--current-symbol-bounds))
                   (and (not (eq prettify-symbols-unprettify-at-point 'right-edge))
                        (= (point) (cadr prettify-symbols--current-symbol-bounds)))))
      (apply #'font-lock-flush prettify-symbols--current-symbol-bounds)
      (setq prettify-symbols--current-symbol-bounds nil))
    ;; Unprettify the current symbol.
    (when-let* ((c (or (get-prop-as-list 'display)
                       (get-prop-as-list 'composition)))
                (s (get-prop-as-list 'prettify-symbols-start))
                (e (get-prop-as-list 'prettify-symbols-end))
                (s (apply #'min s))
                (e (apply #'max e)))
      (with-silent-modifications
        (setq prettify-symbols--current-symbol-bounds (list s e))
        (remove-text-properties s e '(composition nil))
        (remove-text-properties s e '(display nil))
        ))))

(defun +font-lock-icons--parse-args (args)
  (let ((prefix-format +font-lock-icons-default-prefix-format)
        (icon-resolver +font-lock-icons-default-icon-resolver)
        (mode-font-lock-alist '((nil . nil)))
        mode)

    (while args
      (let ((arg (pop args))
            (next-arg (pop args)))

        (pcase arg
          (:prefix-format
           (setq prefix-format
                 (cond ((or (equal "" next-arg)
                            (null next-arg))
                        +font-lock-icons-default-prefix-format)
                       ((stringp next-arg) next-arg)
                       (t (error ":prefix-format must be a string or nil" next-arg)))))
          (:icon-resolver
           (if (functionp next-arg)
               (setq icon-resolver next-arg)
             (error ":icon-resolver must be a function")))
          (:mode
           (if (symbolp next-arg)
               (progn
                 (setq mode next-arg
                       icon-resolver +font-lock-icons-default-icon-resolver)
                 (unless (alist-get mode mode-font-lock-alist)
                   (push `(,mode . nil) mode-font-lock-alist)))
             (error ":mode must be a symbol for a mode")
             ))

          ;; Actual mapping
          ((pred stringp)
           (if (stringp next-arg)
               (let ((resolved-pattern (format prefix-format arg))
                     (resolved-icon `(funcall #',icon-resolver ,next-arg)))
                 (setq mode-font-lock-alist (cons
                                             (cons mode (cons
                                                         (list resolved-pattern
                                                               (list 1 `(progn (+prettify--compose (funcall #',icon-resolver ,next-arg)) nil) ))
                                                         (cdr (assq mode mode-font-lock-alist))))
                                             (assoc-delete-all mode mode-font-lock-alist))))
             (error "Icon must be a string")))
          (_ (error "Unsupported key or type: '%s'" arg)))))
    mode-font-lock-alist))

(defun +font-lock-icons! (&rest args)
  (let ((mode-lock-list (+font-lock-icons--parse-args args)))
    (dolist (mode-lock mode-lock-list)
      (when (cdr mode-lock)
        (font-lock-add-keywords (car mode-lock) (cdr mode-lock))))
    mode-lock-list))

(+font-lock-icons!
   :mode 'org-mode
   :prefix-format "^\\*+ \\(%s\\)"
   "TODO" "⚑"
   "STRT" "⚐"
   "STARTED" "⚐"
   "CANCELED" "✘"
   "DONE" "✔"


   :prefix-format (rx "#+" (or "begin" "BEGIN")
                      "_" (or "src" "SRC" "example" "EXAMPLE")
                      " " (group "%s"))
   :icon-resolver #'all-the-icons-fileicon
   "emacs-lisp" "emacs"
   "elisp" "emacs"
   "bash" "terminal"
   "sh" "terminal"

   :icon-resolver #'all-the-icons-alltheicon
   "python" "python"
   )
