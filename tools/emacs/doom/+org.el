;;; ~/Projects/dotfiles/src/config/doom/+org.el -*- lexical-binding: t; -*-


(defun sp-insert-newline-inside-pair (_id action _context)
  "ID, ACTION, CONTEXT."
  (when (eq action 'insert)
    (insert "\n\n")
    (backward-char 1)
    (insert-tab nil)))

(defun org-in-latex-block-p ()
  (let* ((element (org-element-at-point))
         (element-type (org-element-type element)))
    (or
     (and (eq element-type 'src-block)
          (equal (org-element-property ':language element) "latex"))
     (and (eq element-type 'export-block)
          (equal (org-element-property ':type element) "latex"))
     (and (eq element-type 'example-block)
          (string-match-p (rx line-start "latex") (org-element-property ':switches element))))))

(defun sp-org-in-non-latex-block (_id _action _context)
  "ID, ACTION, CONTEXT."
  (let ((element-type (org-element-at-point)))
    (and (memq element-type '(src-block export-block example-block))
     (not (org-in-latex-block-p)))))

(use-package! smartparens-latex
  :after org
  :defer-incrementally t
  :config
  (sp-with-modes '(org-mode)
    (sp-local-pair "`" "'"
                   :actions '(:rem autoskip)
                   :skip-match 'sp-latex-skip-match-apostrophe
                   :unless '(sp-latex-point-after-backslash sp-in-math-p sp-org-in-non-latex-block))
    ;; math modes, yay.  The :actions are provided automatically if
    ;; these pairs do not have global definitions.
    (sp-local-pair "$" "$"
                   :unless '(sp-org-in-non-latex-block))
    (sp-local-pair "\\[" "\\]"
                   :unless '(sp-latex-point-after-backslash sp-org-in-non-latex-block)
                   :post-handlers '(sp-insert-newline-inside-pair))

    ;; disable useless pairs.
    (sp-local-pair "\\\\(" nil :actions nil)
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "\\\"" nil :actions nil)

    ;; quote should insert ``'' instead of double quotes.  If we ever
    ;; need to insert ", C-q is our friend.
    (sp-local-pair "``" "''"
                   :trigger "\""
                   :unless '(sp-latex-point-after-backslash sp-in-math-p sp-org-in-non-latex-block)
                   :post-handlers '(sp-latex-skip-double-quote))

    ;; add the prefix function sticking to {} pair
    (sp-local-pair "{" nil :prefix "\\\\\\(\\sw\\|\\s_\\)*")

    ;; do not add more space when slurping
    (sp-local-pair "{" "}")
    (sp-local-pair "(" ")")
    (sp-local-pair "[" "]")

    ;; pairs for big brackets.  Needs more research on what pairs are
    ;; useful to add here.  Post suggestions if you know some.
    (sp-local-pair "\\left(" "\\right)"
                   :trigger "\\l("
                   :when '(sp-in-math-p)
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\left[" "\\right]"
                   :trigger "\\l["
                   :when '(sp-in-math-p)
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\left\\{" "\\right\\}"
                   :trigger "\\l{"
                   :when '(sp-in-math-p)
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\left|" "\\right|"
                   :trigger "\\l|"
                   :when '(sp-in-math-p)
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\bigl(" "\\bigr)"
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\biggl(" "\\biggr)"
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\Bigl(" "\\Bigr)"
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\Biggl(" "\\Biggr)"
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\bigl[" "\\bigr]"
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\biggl[" "\\biggr]"
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\Bigl[" "\\Bigr]"
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\Biggl[" "\\Biggr]"
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\bigl\\{" "\\bigr\\}"
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\biggl\\{" "\\biggr\\}"
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\Bigl\\{" "\\Bigr\\}"
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\Biggl\\{" "\\Biggr\\}"
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\lfloor" "\\rfloor"
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\lceil" "\\rceil"
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair "\\langle" "\\rangle"
                   :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair  "\\lVert" "\\rVert"
                    :when '(sp-in-math-p)
                    :trigger "\\lVert"
                    :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair  "\\lvert" "\\rvert"
                    :when '(sp-in-math-p)
                    :trigger "\\lvert"
                    :post-handlers '(sp-latex-insert-spaces-inside-pair))

    ;; some common wrappings
    (sp-local-tag "\\b" "\\begin{_}" "\\end{_}")
    (sp-local-tag "bi" "\\begin{itemize}" "\\end{itemize}")
    (sp-local-tag "be" "\\begin{enumerate}" "\\end{enumerate}")))

(after! org
  (setq org-export-async-init-file (expand-file-name "org-export-init.el" (dir!)))

  (use-package! org-crypt
    :after org
    :defer-incrementally t)
  (use-package! ox-extra
    :after org
    :defer-incrementally t)

  (require! config-org
            config-org-latex
            config-org-babel
            config-org-export
            config-org-ui)

  ;; A start, but I want to add a lot more rotations!
  ;; - block type
  ;; - option 'yes', 'no'
  ;; - results types (output, etc)
  ;; - list rotation, todo rotation (might make sense to create a DWIM rotation)
  (set-rotate-patterns! 'org-mode
    :symbols `(
               ;; TODO doesn't like compose, figure out why..
               ,(sort (cons "emacs-lisp"
                            (mapcar (-compose #'downcase #'car) org-src-lang-modes))
                      #'string<)
               ("yes" "no"))
    :patterns (list (cons
                     (rx line-start
                         "#+"
                         (group (+ word))
                         ":")
                     (sort (cl-delete-duplicates
                            (mapcar (lambda (keyword)
                                      (concat "#+" keyword))
                                    org-options-keywords)
                            :test #'equal)
                           #'string<))))

  ;; Make my org directory be a project root
  (defun +org-notes-root (dir)
    (and dir
         (file-in-directory-p dir org-directory)
         org-directory))
  (after! projectile
    (add-to-list 'projectile-project-root-files-functions '+org-notes-root))

  (setq +ligatures-extra-symbols (append
                                  `(:title    ,(propertize "" 'display '(raise 1))
                                    :author   ,(propertize "" 'display '(raise 0.1))
                                    :setting  ,(propertize "" 'display '(raise 0.1))
                                    :latex    ,(all-the-icons-fileicon "tex")
                                    :property ,(all-the-icons-octicon "chevron-right"))
                                  +ligatures-extra-symbols))

  (set-ligatures! 'org-mode
    :src_block     "#+begin_example"
    :src_block_end "#+end_example"
    :src_block     "#+BEGIN_EXAMPLE"
    :src_block_end "#+END_EXAMPLE"
    :title         "#+TITLE:"
    :setting       "#+PROPERTY:"
    :author        "#+AUTHOR:"
    :latex         "#+LATEX_HEADER:"
    :latex         "#+LATEX_HEADER_EXTRA:"
    :property      "#+STARTUP:"
    :property      "#+RESULTS:")

  (add-hook 'org-mode-hook
            #'(lambda () (yas-activate-extra-mode 'latex-mode))))


(set-file-template! 'org-mode
  :when '(lambda (file)
           (file-in-directory-p file "~/homework"))
  :trigger "__hw"
  :mode 'org-mode)

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
  (cl-destructuring-bind
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
       "<M-S-return>" '+org/M-S-Return
       "<normal-state> <M-return>" '+tw/org-ctrl-c-ret)
      (:map evil-org-mode-map
       :vn "zn" #'+evil:narrow-buffer))

;;; Babel
(defadvice! +org-babel--buffer-arg (fn result &optional result-params info hash lang)
  "Popup results of an org src block if :buffer t is given as a header argument "
  :around  'org-babel-insert-result ;; probably a better type of advice
  (funcall fn result result-params info hash lang)
  ;; (message "%s" (pp-to-string info))
  (when-let ((buffer-val (alist-get :buffer (nth 2 info))))
    (let* ((result-buffer-name (concat "*org:results:" (buffer-name) "["  hash "]*" ))
           (result-buffer (get-buffer-create result-buffer-name)))
      (with-current-buffer result-buffer
        (erase-buffer)
        (insert result)
        (+popup-buffer (current-buffer))))))
