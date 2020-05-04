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

(defadvice! +counsel-projectile-find-file (&optional arg dwim)
  :override 'counsel-projectile-find-file
  "Jump to a file in the current project.

With a prefix ARG, invalidate the cache first.  If DWIM is
non-nil, use completion based on context."
  (interactive "P")
  (if (and (eq projectile-require-project-root 'prompt)
           (not (projectile-project-p)))
      (counsel-projectile-find-file-action-switch-project)
    (projectile-maybe-invalidate-cache arg)
    (let* ((project-files (projectile-current-project-files))
           (files (and dwim (projectile-select-files project-files))))
      (ivy-read (projectile-prepend-project-name "Find file: ")
                (or files project-files)
                :matcher counsel-projectile-find-file-matcher
                :sort counsel-projectile-sort-files
                :action counsel-projectile-find-file-action
                :caller '+counsel-projectile-find-file))))

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
