;;; .doom.d/config.el -*- lexical-binding: t; -*-

(load! "bootstrap")

(use-package! dash-functional
  :commands (-compose))

(use-package! rextract
  :defer-incrementally t)

(defvar personal/config-directory (expand-file-name "~/Projects/dotfiles"))

(+load!?
 ;; General
 "+reset"
 "+core"
 "+svg"
 "+doom"
 "+ui"
 "+key"
 "+movement"
 ;; Features / tools / etc
 "+bookmark"
 "+ivy"
 "+evil"
 "+elisp"
 "+org"
 "+eshell"
 "+magit"
 "+pdf"
 "+tmux"
 ;;lang
 "+c"
 "+php"
 "+scheme"
 ;; Hacks
 "+hacks"
 )


(after! undo-fu
  (remove-hook 'undo-fu-mode-hook #'global-undo-fu-session-mode))


(setq
 find-function-C-source-directory (expand-file-name "~/src/emacs/src"))

(use-package! howdoyou
  :defer-incrementally t)

(remove-hook! '(org-mode-hook) #'flyspell-mode)

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
(put 'erase-buffer 'disabled nil)
