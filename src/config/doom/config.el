;;; .doom.d/config.el -*- lexical-binding: t; -*-

(defun load!? (filename)
  (if (file-exists-p! filename (dir!))
      (load! filename)
    (if (file-exists-p! (concat filename ".el") (dir!))
        (load! (concat filename ".el")))))

(defun +load!? (&rest files)
  "Check if the file exists in the (dir!) first and then (load!)"
  (mapcar 'load!? files))

(+load!?
 "+core"
 "+org"
 "+key")

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

;; (use-package! emojify
;;   :config
;;   (add-hook 'after-init-hook #'global-emojify-mode))

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


;; Allow local configuration
(load!? "+profile")
