;;; ~/Projects/dotfiles/src/config/doom/+org.el -*- lexical-binding: t; -*-

(use-package! org-crypt
  :after org
  :defer-incrementally t
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt"))
        org-crypt-key nil))

(after! org
  (setq
    org-src-fontify-natively t
    org-todo-keywords '((sequence "TODO" "STARTED" "|" "DONE" "CANCELED"))
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
    org-src-window-setup 'current-window
    )

    (defun map-alist (f alist)
      (mapcar (lambda (key-val)
                (setq key (car key-val)
                      val (cdr key-val))
                (funcall f key val))
              alist))

    (defun org-mode-todo-symbols (todo-alist)
      (setq org-todo-font-lock-replace
            (map-alist (lambda (keyword symbol)
                         `(,(concat "^\\*+ \\(" keyword "\\)")
                           (1 (progn (compose-region (match-beginning 1) (match-end 1) ,symbol) nil))))
                       todo-alist))

    (font-lock-add-keywords
     'org-mode org-todo-font-lock-replace))

    (org-mode-todo-symbols
     '(("TODO" . "⚑")
       ("STRT" . "⚐")
       ("STARTED" . "⚐")
       ("CANCELED" . "✘")
       ("DONE" . "✔")))

    (defun +tw/org-ctrl-c-ret ()
      (interactive)
      (org-ctrl-c-ret)
      (evil-insert-state))



  )

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

(set-pretty-symbols! 'org-mode
    :name "#+name:"
    :src_block "#+begin_src"
    :src_block_end "#+end_src")

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
       ))
