;;; Tyler Ware's Org Layer -*- lexical-binding: t -*-

(setq tw-org-packages
  '(org
      ;; Owned packages
      org-drill
      (pretty-todo :location local)))

(defun tw-org/init-pretty-todo ()
  (use-package pretty-todo))

(defun tw-org/init-org-drill ()
  (use-package org-drill))


(defun tw-org/post-init-org ()
  (setq
    org-directory "~/org"
    org-modules '(
                  org-bbdb
                  org-bibtex
                  org-docview
                  org-drill
                  org-eww
                  org-gnus
                  org-info
                  org-irc
                  org-mhe
                  org-rmail
                  org-w3m
                  )
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
    org-plantuml-jar-path (expand-file-name "~/java/plantuml/plantuml.jar")
    org-display-inline-images t
    org-pretty-entities t
    org-startup-with-inline-images "inlineimages")

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((http . t)
     (restclient . t)
     (plantuml . t)
     (sql . t)
     (js . t)
     (shell . t)
     (python . t)
     (emacs-lisp . t)))

  (defun map-alist (f alist)
    (mapcar (lambda (key-val)
              (setq key (car key-val)
                    val (cdr key-val))
              (funcall f key val))
            alist))

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

)
