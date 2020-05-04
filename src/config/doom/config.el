;;; .doom.d/config.el -*- lexical-binding: t; -*-

(load! "bootstrap")

(+load!?
 ;; General
 "+reset"
 "+core"
 "+doom"
 "+ui"
 "+key"
 ;; Features / tools / etc
 "+evil"
 "+elisp"
 "+org"
 "+eshell"
 "+tmux"
 )


(setq
 find-function-C-source-directory (expand-file-name "~/src/emacs/src"))

(use-package! howdoyou
  :defer-incrementally t)

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
