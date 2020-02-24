;;; ~/Projects/dotfiles/src/config/doom/+org.el -*- lexical-binding: t; -*-

(use-package! org-crypt
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt"))
        org-crypt-key nil))

(after! org
  (setq
    ;; org-modules '(
    ;;               org-bbdb
    ;;               org-bibtex
    ;;               org-docview
    ;;               org-drill
    ;;               org-eww
    ;;               org-gnus
    ;;               org-info
    ;;               org-irc
    ;;               org-mhe
    ;;               org-rmail
    ;;               org-w3m
    ;;               )
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
     '(("TODO"	. "⚑")
       ("STRT"	. "⚐")
       ("STARTED"	. "⚐")
       ("CANCELED"	. "✘")
       ("DONE"	. "✔")))

    (defun +tw/org-ctrl-c-ret ()
      (interactive)
      (org-ctrl-c-ret)
      (evil-insert-state))

    (general-define-key
     :keymaps 'org-mode-map
     "<S-return>" 'org-ctrl-c-ctrl-c
     "<normal-state> <M-return>" '+tw/org-ctrl-c-ret
     )

(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))
  )
