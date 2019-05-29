;;; Tyler Ware's Org Layer -*- lexical-binding: t -*-

(setq tw-org-packages
  '(
      org

      ;; Owned packages
      (pretty-todo :location local)))

(defun tware-org/init-pretty-todo ()
  (use-package pretty-todo))


(defun tware-org/post-init-org ()
  (setq
    org-agenda-files (f-entries (expand-file-name "~/org") (lambda (filename) (s-ends-with-p ".org" filename)) t)
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
  ))
