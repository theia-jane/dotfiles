
;; OS config tools

; ;; Swapping
; (setq use-spacemacs nil)   ; or nil 
; (when use-spacemacs
;     (setq user-emacs-directory "~/.spacemacs.d/"))   ; defaults to ~/.emacs.d/
; (load (expand-file-name "init.el" user-emacs-directory))


;; Package setup (using 'use-package)
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

(setq
 my-dotfiles (expand-file-name "~/.config/personal/dotfiles/")
 my-org (expand-file-name "~/org/"))

(defun is-system (system-name) (eq system-type system-name))
(defun is-mac () (is-system 'darwin))
(defun is-linux () (is-system 'gnu/linux))
(defun system-cond ()
  (cond ((is-mac) mac)
        ((is-linux) linux)))

(use-package f :ensure t)
(use-package s :ensure t)
(use-package a :ensure t)
(use-package @ :ensure t)

;; Key bindings!
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

(use-package atomic-chrome :ensure t)
;; Workspaces
(use-package perspective
  :ensure t
  :config
  (persp-mode t))

(use-package powerline :ensure t)
(use-package powerline-evil :ensure t
  :config
  (powerline-evil-center-color-theme))

;; Shut up the bell
(defun my-bell-function ())
(setq ring-bell-function 'my-bell-function)
(setq visible-bell nil)

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
(use-package restclient :ensure t)

;; only the evilest
(use-package evil :ensure t
             :config
             (evil-mode 1))

;; (use-package evil-org :ensure t)

(use-package which-key :ensure t
  :config
  (which-key-mode 1))

;; No intial start screen 
(setq inhibit-startup-screen t)

;; No bars on top
(menu-bar-mode 0)
(tool-bar-mode 0)

;; no GUI scroll bars
(scroll-bar-mode 0)


;; Set font niceness
(set-default-font "Source Code Pro-14")

(use-package darktooth-theme :ensure t :config (load-theme 'darktooth t))

;(use-package gruvbox-theme :ensure t :config (load-theme 'gruvbox t))

;; Example of telling when in terminal (need to still figure out emacs client)
;; https://emacs.stackexchange.com/questions/13050/different-theme-for-nw-terminal
;; https://emacs.stackexchange.com/questions/2096/different-themes-for-terminal-and-graphical-frames-when-using-emacs-daemon


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

;; Fuzzy finding for emacs
(use-package flx :ensure t)

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

;; Org niceness
(use-package org-bullets :ensure t
  :init (add-hook 'org-mode-hook 'org-bullets-mode))
(use-package worf :ensure t
  :init (add-hook 'org-mode-hook 'worf-mode))
;(general )
;~/.config/personal/dotfiles/emacs/init.el

;; Example of loading & parsing some JSON
;; https://emacs.stackexchange.com/questions/27407/accessing-json-data-in-elisp
;; (require 'json)
;; (json-read-file "~/.mappings.json")


(use-package markdown-mode :ensure t)

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

;;    (define-key evil-insert-state-map (kbd "C-.") "hello")

;;(setq yas-snippet-dirs
 ;;  '(
  ;;   "~/.config/personal/snippets"
   ;;))

(setq backup-by-copying-when-linked t)

(setq mode-require-final-newline nil)

;(defun append-to-list (list-var elements)
     ;"Append ELEMENTS to the end of LIST-VAR.
;
     ;The return value is the new value of LIST-VAR."
       ;(unless (consp elements)
         ;(error "ELEMENTS must be a list"))
       ;(let ((list (symbol-value list-var)))
         ;(if list
             ;(setcdr (last list) elements)
           ;(set list-var elements)))
       ;(symbol-value list-var))

;; (slack-register-team
;;  :name ""
;;  :default t
;;  :client-id ""
;;  :client-secret ""
;;  :token ""
;;  )

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

;(setq ns-pop-up-frames nil)
;(setq ns-use-srgb-colorspace nil)
;(setq ns-pop-up-frames nil)

(setq powerline-default-separator 'slant)

(setq 
  evil-overriding-maps nil
  evil-intercept-maps nil)

(use-package matlab-mode :ensure t)
