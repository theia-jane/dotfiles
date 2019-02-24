(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  ;; Gen funcs
  (defun map-alist (f alist)
    (mapcar (lambda (key-val)
              (setq key (car key-val)
                    val (cdr key-val))
              (funcall f key val))
            alist))

  ;; Env vars:
  (setq
    my-config (expand-file-name "~/.config/personal/")
    my-dotfiles (expand-file-name "~/.config/personal/dotfiles/")
    org-root (expand-file-name "~/org/")
    my-org (expand-file-name  "~/org/"))

  ;; Org things:
  (setq
  org-agenda-files (f-entries my-org (lambda (filename) (s-ends-with-p ".org" filename)) t)
  org-directory "~/org"
  org-modules (append org-modules '(org-drill))
  org-src-fontify-natively t
  org-todo-keywords '((sequence "TODO" "STARTED" "|" "DONE" "CANCELED"))
  org-capture-templates
  '(("p" "Plain" entry (file "") "* %?")
    ("t" "Todo" entry (file "") "* TODO %?"))
  org-refile-targets '((nil :maxlevel . 7)
                        (org-agenda-files :maxlevel . 1))
  org-refile-allow-creating-parent-nodes t
  org-outline-path-complete-in-steps nil    ; Refile in a single go
  org-refile-use-outline-path 'file         ; Show full paths for refiling
  org-highlight-latex-and-related '(latex)
  org-M-RET-may-split-line '((default . nil)) ; don't split headings...
  org-src-tab-acts-natively t
  org-confirm-babel-evaluate nil
  )


  (defun org-mode-todo-symbols (todo-alist)
    (setq org-todo-font-lock-replace
          (map-alist (lambda (keyword symbol)
                      `(,(concat "^\\*+ \\(" keyword "\\) ")
                        (1 (progn (compose-region (match-beginning 1) (match-end 1) ,symbol) nil))))
                    todo-alist))

    (font-lock-add-keywords
    'org-mode org-todo-font-lock-replace))

  (org-mode-todo-symbols
    '(("TODO" . "⚑")
      ("STARTED" .  "⚐")
      ("CANCELED" .  "✘")
      ("DONE" .  "✔")))

  (spacemacs/declare-prefix "o" "own-menu")
  (spacemacs/set-leader-keys "oof" '(lambda () (interactive) (counsel-find-file org-root)))
  (spacemacs/set-leader-keys "odf" '(lambda () (interactive) (counsel-find-file my-dotfiles)))
  )
