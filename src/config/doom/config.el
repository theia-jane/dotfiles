;;; .doom.d/config.el -*- lexical-binding: t; -*-

;;; NOTE: (use-package!) should be used with care and specifically with defering keywords
;;;       Using use-package! without a deferring keyword (one of: :defer :after
;;;       :commands :defer-incrementally :after-call) will load the package
;;;       immediately. This can cause other packages to be pulled in and loaded,
;;;       which will compromise many of Doom’s startup optimizations.
(use-package! all-the-icons-ivy
  :config
  (all-the-icons-ivy-setup))

(use-package! all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package! try)

(set-irc-server! "chat.freenode.net"
  `(:tls t
    :port 6697
    :nick "tylerware"
    :sasl-username "tylerware"
    :sasl-password ,(lambda (&rest _) (+pass-get-secret "irc/freenode.net"))
    :channels ("#emacs")))

(defun tw/flyspell-save-word ()
  (interactive)
  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

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
    org-highlight-latex-and-related '(latex)
    org-M-RET-may-split-line '((default . nil)) ; don't split headings...
    org-src-tab-acts-natively t
    org-confirm-babel-evaluate nil
    org-plantuml-jar-path (expand-file-name "~/java/plantuml/plantuml.jar")
    org-display-inline-images t
    org-pretty-entities t
    org-startup-with-inline-images "inlineimages"
    org-hide-emphasis-markers nil
    org-export-with-toc nil
    org-export-with-section-numbers nil
    org-export-time-stamp-file nil
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
       ("STARTED"	. "⚐")
       ("CANCELED"	. "✘")
       ("DONE"	. "✔")))
    (defun +tw/org-ctrl-c-ret ()
      (interactive)
      (org-ctrl-c-ret)
      (evil-insert-state))
  )

(after! dired
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

;; General Visual
(defun config/visual/buffer-spacing ()
  (setq-default
   line-spacing 7
   ;; header-line-format " "                                ; Add content to header line
   left-margin-width 3
   right-margin-width 3
   )
  ;; (set-face-attribute 'header-line nil :background "15") ; Set the background color of the header line to the background of the theme
  (set-window-margins nil 2 2)                           ; Add padding at the sides
 )

(defun config/visual ()
    (config/visual/buffer-spacing))

;; General Key
(defun config/key ()
  (general-define-key
   :keymaps 'evil-normal-state-map
   "g f" (lambda () (interactive)
           (let ((file (ffap-guess-file-name-at-point)))
             (when file
               (find-file file))))
   )

  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "t h" 'evil-ex-nohighlight
  )
  )



(defun config/init ()
  (config/visual)
  (config/key))

(config/init)

(defun tw/new-org-scratch ()
  "Create a new empty buffer.
New buffer will be named “org-scratch” or “org-scratch<2>”, “org-scratch<3>”, etc.

It returns the buffer (for elisp programing).
"
  (interactive)
  (let (($buf (generate-new-buffer "org-scratch")))
    (switch-to-buffer $buf)
    (org-mode)
    (setq buffer-offer-save t)
    $buf
    ))
