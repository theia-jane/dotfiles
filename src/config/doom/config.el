;;; .doom.d/config.el -*- lexical-binding: t; -*-

(load! "+core.el")
(load! "+org.el")
(load! "+key.el")

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

(config/visual/buffer-spacing)

(setq doom-scratch-buffer-major-mode 'emacs-lisp-mode)

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

;; Allow local configuration
(load!? "+profile.el")
