;; OS config tools

; (defun get-system-type () system-type)
; (defun is-os (os-name) (eq (get-system-type) os-name))
; (defun is-mac () (is-os 'darwin))
; (defun is-linux() (is-os 'gnu/linux))
;
; (defun os-switch (mac linux)
;   (cond ((is-mac) mac)
;         ((is-linux) linux)))
;
;
; ;; Swapping
; (setq use-spacemacs nil)   ; or nil 
; (when use-spacemacs
;     (setq user-emacs-directory "~/.spacemacs.d/"))   ; defaults to ~/.emacs.d/
; (load (expand-file-name "init.el" user-emacs-directory))


(defun is-system (system-name) (eq system-type system-name))
(defun is-mac () (is-system 'darwin))
(defun is-linux () (is-system 'gnu/linux))

(defun system-cond ()
  (cond ((is-mac) mac)
	((is-linux) linux)))

;; Package setup (using 'use-package)
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
			 ("gnu" . "https:/elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(use-package general :ensure t)

;; Only the evilest
(use-package evil :ensure t
	     :config
	     (evil-mode 1)
	     )

(use-package evil-org :ensure t)
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
(use-package spacegray-theme :ensure t
  :config
  (load-theme 'spacegray t))

(use-package org-bullets :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )

;; (use-package org-beautify-theme :ensure t)

;; Example of telling when in terminal (need to still figure out emacs client)
;; https://emacs.stackexchange.com/questions/13050/different-theme-for-nw-terminal
;; https://emacs.stackexchange.com/questions/2096/different-themes-for-terminal-and-graphical-frames-when-using-emacs-daemon


;; Finding / Narrowing / Completing 
(use-package ivy :ensure t
  :config
  ;; clears the intial ^ when using ivy
  (setq ivy-initial-inputs-alist nil))

(use-package counsel :ensure t
  :bind*
  (("M-x" . counsel-M-x)
   ("C-x f" . counsel-find-file)))


;; Example of loading & parsing some JSON
;; https://emacs.stackexchange.com/questions/27407/accessing-json-data-in-elisp
;; (require 'json)
;; (json-read-file "~/.mappings.json")
