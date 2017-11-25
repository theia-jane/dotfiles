;; OS config tools
(defun get-system-type () system-type)
(defun is-os (os-name) (eq (get-system-type) os-name))
(defun is-mac () (is-os 'darwin))
(defun is-linux() (is-os 'gnu/linux))

(defun os-switch (mac linux)
  (cond ((is-mac) mac)
        ((is-linux) linux)))


;; Swapping
(setq use-spacemacs nil)   ; or nil 
(when use-spacemacs
    (setq user-emacs-directory "~/.spacemacs.d/"))   ; defaults to ~/.emacs.d/
(load (expand-file-name "init.el" user-emacs-directory))
