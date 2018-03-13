
(use-package exec-path-from-shell 
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (defun source-file-and-get-envs (filename)
    (let* ((cmd (concat ". " filename "; env"))
           (env-str (shell-command-to-string cmd))
           (env-lines (split-string env-str "\n"))
           (envs (mapcar (lambda (s) (replace-regexp-in-string "=.*$" "" s)) env-lines)))
      (delete "" envs)))
  (exec-path-from-shell-copy-envs (source-file-and-get-envs "~/.profile")))

(setq
 my-dotfiles (expand-file-name "~/.config/personal/dotfiles/")
 my-org (expand-file-name "~/org/"))

(defun is-system (system-name) (eq system-type system-name))
(defun is-mac () (is-system 'darwin))
(defun is-linux () (is-system 'gnu/linux))
(defun system-cond ()
  (cond ((is-mac) mac)
        ((is-linux) linux)))

(use-package flx :ensure t)

(use-package f :ensure t)
(use-package s :ensure t)
(use-package a :ensure t)
(use-package @ :ensure t)

(require 'package)
(package-initialize)

(setq package-enable-at-startup nil)
(setq package-archives '(("org" . "https://orgmode.org/elpa/")
                        ("gnu" . "https://elpa.gnu.org/packages/")
                        ("melpa" . "https://melpa.org/packages/")
                        ("marmalade" . "https://marmalade-repo.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(setq inhibit-startup-screen t)

(defun my-bell-function ())
(setq ring-bell-function 'my-bell-function)
(setq visible-bell nil)

(set-default-font "Source Code Pro-14")

(use-package darktooth-theme 
  :ensure t 
  :config 
  (load-theme 'darktooth t))

(use-package powerline :ensure t)
(use-package powerline-evil :ensure t
  :config
  (powerline-evil-center-color-theme)
  (setq powerline-default-separator 'slant))

(use-package general :ensure t
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

   "SPC"   'counsel-M-x
   "f d e" '(lambda () (interactive) (find-file (concat my-dotfiles "emacs/index.org")))
   "f d f" '(lambda () (interactive) (counsel-find-file my-dotfiles))
   "f o f" '(lambda () (interactive) (counsel-find-file my-org))
   "f f" 'counsel-find-file

   "b b" 'ivy-switch-buffer
   ))

(use-package perspective
    :ensure t
    :config
    (persp-mode t))

(recentf-mode)

(use-package which-key :ensure t
:config
(which-key-mode 1))

(use-package markdown-mode :ensure t)

(use-package haskell-mode :ensure t)

(use-package web-mode :ensure t
  :config
  (add-to-list 'auto-mode-alist (cons ".*\\.html" 'web-mode ))
  (add-to-list 'auto-mode-alist (cons ".*\\.php$" 'web-mode )))

;(use-package matlab-mode :ensure t)

(show-paren-mode t)

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :config
  (dolist
      (mode-hook '(emacs-lisp-mode-hook
                   eval-expression-minibuffer-setup-hook
                   ielm-mode-hook
                   lisp-mode-hook
                   lisp-interaction-mode-hook
                   scheme-mode-hook))
    (add-hook mode-hook #'enable-paredit-mode)))

(use-package evil-paredit :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode))

;; Finding / Narrowing / Completing 
(use-package ivy :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  ;; clears the intial ^ when using ivy
  (setq ivy-initial-inputs-alist nil
        ivy-re-builders-alist
         '((ivy-switch-buffer . ivy--regex-plus)
           (t . ivy--regex-fuzzy)))
  :bind*
  (("C-x b" . ivy-switch-buffer)
   ("C-x C-b" . ivy-switch-buffer))
  :bind (:map ivy-minibuffer-map
              ("C-n" . ivy-next-history-element)
              ("C-p" . ivy-previous-history-element)
              ("C-k" . ivy-previous-line)
              ("C-j" . ivy-next-line)
              ))

(use-package counsel :ensure t
  :bind
  (("M-x" . counsel-M-x)
   ("C-x f" . counsel-find-file)))

(use-package restclient 
  :ensure t)

;; (slack-register-team
;;  :name ""
;;  :default t
;;  :client-id ""
;;  :client-secret ""
;;  :token ""
;;  )

;; Org niceness
(use-package org-bullets :ensure t
  :init (add-hook 'org-mode-hook 'org-bullets-mode))
(use-package worf :ensure t
  :init (add-hook 'org-mode-hook 'worf-mode))
;; mapping an associative list
(defun map-alist (f alist)
    (mapcar (lambda (key-val)
            (setq key (car key-val)
                    val (cdr key-val))
            (funcall f key val))
            alist))

;; Map keywords (TODO) to a nicer icon 
(defun org-mode-todo-symbols (todo-alist)
  (setq org-todo-font-lock-replace
        (map-alist (lambda (keyword symbol)
                 `(,(concat "^\\*+ \\(" keyword "\\) ") 
                   (1 (progn (compose-region (match-beginning 1) (match-end 1) ,symbol) nil))))
               todo-alist))

  (font-lock-add-keywords            
   'org-mode org-todo-font-lock-replace))


(use-package org
  :ensure t
  :config
  (org-mode-todo-symbols
   '(("TODO" . "⚑")
     ("DOING" .  "⚐")
     ("CANCELED" .  "✘")
     ("DONE" .  "✔"))))

(setq
 org-log-into-drawer "logbook"
 org-agenda-files (f-entries my-org (lambda (filename) (s-ends-with-p ".org" filename)) t)
 org-directory "~/org"
 org-modules (append org-modules '(org-drill)))

(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "|" "DONE" "CANCELED")))

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (emacs-lisp . t)
   ;;(org . t)
   ;;(js . t)
   ;;(latex . t)
   ;; (php . t)
   ;;(dot . t)
   ;;(shell . t)
   ))

(setq org-capture-templates '(
                              ("p" "Plain" entry (file "")
                               "* %?")
                              ("t" "Todo" entry (file "")
                               "* TODO %?")
                              ))

;      (setq org-mobile-inbox-for-pull "~/Nextcloud/org/flagged.org")
;      (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

(setq org-refile-targets '((nil :maxlevel . 7)
                          (org-agenda-files :maxlevel . 1))
      org-refile-allow-creating-parent-nodes t
      org-outline-path-complete-in-steps nil    ; Refile in a single go
      org-refile-use-outline-path 'file)        ; Show full paths for refiling

(setq org-highlight-latex-and-related '(latex))

(setq org-M-RET-may-split-line '((default . nil)))

;; Example of loading & parsing some JSON
;; https://emacs.stackexchange.com/questions/27407/accessing-json-data-in-elisp
;; (require 'json)
;; (json-read-file "~/.mappings.json")

;;    (define-key evil-insert-state-map (kbd "C-.") "hello")

;;(setq yas-snippet-dirs
 ;;  '(
  ;;   "~/.config/personal/snippets"
   ;;))

(setq backup-by-copying-when-linked t)

(setq mode-require-final-newline nil)

(use-package evil :ensure t
  :config
  (evil-mode 1)
  (setq 
   evil-overriding-maps nil
   evil-intercept-maps nil))

(use-package evil-surround
  :ensure t
  :config
   (global-evil-surround-mode 1))

(if  (file-exists-p  "~/.emacs.local.org")
    (org-babel-load-file "~/.emacs.local.org"))
