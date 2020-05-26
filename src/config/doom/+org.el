;;; ~/Projects/dotfiles/src/config/doom/+org.el -*- lexical-binding: t; -*-

(use-package! org-crypt
  :after org
  :defer-incrementally t
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt"))
        org-crypt-key nil))

(after! org
  (setq org-todo-keywords '((sequence "TODO" "STARTED" "|" "DONE" "CANCELED"))
        org-capture-templates
        '(("p" "Plain" entry (file "") "* %?")
          ("t" "Todo" entry (file "") "* TODO %?"))
        org-refile-targets '((nil :maxlevel . 7)
                             (org-agenda-files :maxlevel . 1))
        org-refile-allow-creating-parent-nodes t
        org-src-ask-before-returning-to-edit-buffer nil
        org-outline-path-complete-in-steps nil    ; Refile in a single go
        org-refile-use-outline-path 'file         ; Show full paths for refiling
        org-highlight-latex-and-related nil ;; '(latex)
        org-M-RET-may-split-line '((default . nil)) ; don't split headings...
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil
        org-plantuml-jar-path (expand-file-name "~/java/plantuml/plantuml.jar")
        org-display-inline-images t
        org-pretty-entities nil
        org-startup-with-inline-images "inlineimages"
        org-hide-emphasis-markers nil
        org-export-with-toc nil
        org-export-with-section-numbers nil
        org-export-time-stamp-file nil
        org-export-async-init-file (expand-file-name "org-export-init.el" (dir!))
        org-directory "~/notes"
        org-src-window-setup 'current-window))


(use-package! ox-extra
  :after org
  :defer-incrementally t
  :config
  (ox-extras-activate '(ignore-headlines)))

(set-file-template! 'org-mode
  :when '(lambda (file)
           (file-in-directory-p file "~/homework"))
  :trigger "__hw"
  :mode 'org-mode)

(setq +pretty-code-symbols (append
                            `(:title ,(propertize "" 'display '(raise 1))
                              :author ,(propertize "" 'display '(raise 0.1))
                              :setting ,(propertize "" 'display '(raise 0.1))
                              :latex ,(all-the-icons-fileicon "tex")
                              :property ,(all-the-icons-octicon "chevron-right"))
                            +pretty-code-symbols))

(set-pretty-symbols! 'org-mode
  :name "#+name:"
  :src_block "#+begin_src"
  :src_block_end "#+end_src"
  :src_block "#+begin_example"
  :src_block_end "#+end_example"
  :src_block "#+BEGIN_EXAMPLE"
  :src_block_end "#+END_EXAMPLE"
  :title "#+TITLE:"
  :setting "#+PROPERTY:"
  :author "#+AUTHOR:"
  :latex "#+LATEX_HEADER:"
  :latex "#+LATEX_HEADER_EXTRA:"
  :property  "#+STARTUP:"
  :property "#+RESULTS:")

(defun +org/goto-next-src-block ()
  (interactive)
  (org-next-block 1 nil "^[ \t]*#\\+begin_src")
  (+ui/smart-scroll-to-top))

(defun +org/eval-src-block-then-next ()
  (interactive)
  (condition-case _err
      (org-babel-execute-src-block)
    (t
     (+org/goto-next-src-block)
     (org-babel-execute-src-block)))
  (+org/goto-next-src-block))

(defun +tw/org-ctrl-c-ret ()
  (interactive)
  (org-ctrl-c-ret)
  (evil-insert-state))

(defvar +org-babel-eval-in-repl-candidates '("bash" "sh"))

(defun +org/babel-src-get-dir (header-args)
  (let ((dir (cdr (assq :dir header-args))))
    (if (and dir (not (file-remote-p dir)))
        dir
      default-directory)))

(defun +org/babel-eval-in-repl ()
  "TODO"
  (interactive)
  (destructuring-bind
      (lang contents header-args &rest _)
      (org-babel-get-src-block-info)
    (cond ((member lang org-babel-shell-names)
           (if (not (equal lang "eshell"))
               (+org-babel-src-evaluate-as-script
                lang
                (org-babel-expand-body:generic
                 contents header-args (org-babel-variable-assignments:shell header-args))
                header-args))))))

(defun +org-babel-src-evaluate-as-script (lang contents header-args)
  "TODO"
  (let* ((dir (+org/babel-src-get-dir header-args))
         (stdin (let ((stdin (cdr (assq :stdin header-args)))
                      (inhibit-message t))
                  (when stdin
                    (org-babel-sh-var-to-string (org-babel-ref-resolve stdin)))))
         (cmdline (cdr (assq :cmdline header-args)))
         (shebang (or
                   (cdr (assq :shebang header-args))
                   (format "#!/usr/bin/env %s" lang)))
         (script-file (org-babel-temp-file (concat lang "-script-")))
         (stdin-file (org-babel-temp-file "stdin-"))
         (padline (not (string= "no" (cdr (assq :padline header-args))))))
    (with-temp-file script-file
      (insert shebang "\n")
      (when padline (insert "\n"))
      (insert contents))
    (set-file-modes script-file #o755)
    (let ((command-string
           (cond
            ((or stdin cmdline)
             (with-temp-file stdin-file (insert (or stdin "")))
             (concat "cat " stdin-file " | " script-file (and cmdline (concat " " cmdline))))
            (t script-file))))
      (+eshell-run-command-visibly command-string dir))))

(defun +org-can-eval-in-repl? (element)
  "TODO"
  (let* ((type (org-element-type element))
         (lang (org-element-property :language element)))
    (and (eq type 'src-block)
         (member lang +org-babel-eval-in-repl-candidates))))

(defun +org/M-S-Return (arg)
  "Change what happens depending on where the point is."
  (interactive "P")
  (let* ((element (org-element-at-point)))
    (cond ((+org-can-eval-in-repl? element)
           (+org/babel-eval-in-repl))
          (t (org-insert-todo-heading arg)))))

(map! (:map org-mode-map
       "<S-return>" 'org-ctrl-c-ctrl-c
       "<M-S-return>" '+org/M-S-Return
       :ni "<C-S-return>" '+org/eval-src-block-then-next
       :n "M-i" #'org-edit-special
       "<normal-state> <M-return>" '+tw/org-ctrl-c-ret)
      (:map evil-org-mode-map
       :ni "<C-S-return>" '+org/eval-src-block-then-next
       ;; evil-org text objects
       :vo "ae" #'evil-org-an-object
       :vo "ie" #'evil-org-inner-object
       :vo "aE" #'evil-org-an-element
       :vo "iE" #'evil-org-inner-element
       :vo "ir" #'evil-org-inner-greater-element
       :vo "ar" #'evil-org-a-greater-element
       :vo "aR" #'evil-org-a-subtree
       :vo "iR" #'evil-org-inner-subtree
       :vn "zn" #'+evil:narrow-buffer

       ))

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
        (message "post hook %s-%s" s e)
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
