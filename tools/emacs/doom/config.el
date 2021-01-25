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
 "+reset"
 "+core" ;; TODO migrate this to 'personal-lib
 "+svg"
 "+doom"
 "+ui"
 "+key"
 "+movement"
 ;; Features / tools / etc
 "+bookmark"
 "+evil"
 "+elisp"
 "+org"
 "+notes"
 "+eshell"
 "+magit"
 "+pdf"
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
 lean-config
 sagemath-config
 config-selection-completion)

(after! undo-fu
  (remove-hook 'undo-fu-mode-hook #'global-undo-fu-session-mode))

(setq
 find-function-C-source-directory (expand-file-name "~/src/emacs/src"))

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

;; Allow local system configuration
(load!? "~/.local.el")
