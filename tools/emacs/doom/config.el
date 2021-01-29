;;; .doom.d/config.el -*- lexical-binding: t; -*-

(load! "bootstrap")

(use-package! dash-functional
  :commands (-compose))

(use-package! rextract
  :defer-incrementally t)

(use-package! nov
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(defvar personal/config-directory (expand-file-name "~/Projects/dotfiles"))

(+load!?
 ;; General
 "+svg" ;; Move to own pkg
 "+doom"
 "+ui"
 "+key"
 "+movement"
 ;; Features / tools / etc
 "+bookmark"
 "+org"
 "+notes"
 "+eshell"
 "+magit"
 "+snippets"
 "+tmux"
 ;;lang
 "+c"
 "+php"
 "+scheme"
 ;; Hacks
 "+hacks"
 )

(require!
 config-auth
 config-email
 config-file-management
 config-buffer
 config-pdf
 config-elisp
 lean-config
 sagemath-config
 config-selection-completion)

(after! undo-fu
  (remove-hook 'undo-fu-mode-hook #'global-undo-fu-session-mode))

(setq
 find-function-C-source-directory (expand-file-name "~/src/emacs/src"))

;; Allow local system configuration
(load!? "~/.local.el")
