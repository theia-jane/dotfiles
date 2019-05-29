(setq config-init-alist
      '((personal . init-personal)
        (spacemacs . init-spacemacs)
        (doom . init-doom)))

(setq config-repo-alist
      '((spacemacs . "https://github.com/syl20bnr/spacemacs")
        (doom . "https://github.com/hlissner/doom-emacs")))

(setq config-setup-alist
      '((doom . setup-doom)))

(defun init-personal ()
    (org-babel-load-file "~/.config/emacs/index.org"))

(defun init-spacemacs ()
    (load (expand-file-name "init.el" user-emacs-directory)))

(defun init-doom ()
    (load (expand-file-name "init.el" user-emacs-directory)))

(setq config-flag-file "/tmp/use-emacs-config")

(defun get-config-name ()
  "Returns the config name"
  (if (file-exists-p config-flag-file)
      (with-temp-buffer
        (insert-file-contents config-flag-file)
        (buffer-string))
    (progn
      (message "No config-flag-file found. Continuing with personal")
      "spacemacs")))

(defun config-is-valid (config-name)
  (if (alist-get (intern config-name) config-init-alist) t nil))

(defun config-set-use (config-name)
  (if (config-is-valid config-name) (write-region config-name nil config-flag-file)))

(defun config-setup (config-name config-dir)
  (make-directory config-dir t)
  ;; If we have a repo path, then pull down the repo
  (setq repo-path (alist-get (intern config-name) config-repo-alist))
  (if repo-path 
      (shell-command (concat "git clone " repo-path " " config-dir)))
  ;; If we have a setup function, call it
  (setq setup-func (alist-get (intern config-name) config-setup-alist))
  (if setup-func (funcall setup-func)))



(defun get-config-dir (config-name)
  (expand-file-name (concat "~/.config/emacsds/" config-name)))

(defun config-link-emacsd (config-name)
  (setq config-dir (get-config-dir config-name))
  (if (not (file-exists-p config-dir)) (config-setup config-name config-dir)
  (if (file-symlink-p "~/.emacs.d") 
  (delete-file "~/.emacs.d")
  (delete-directory "~/.emacs.d" t))
  (make-symbolic-link config-dir "~/.emacs.d" t)))

(defun config-magit-status ()
  (interactive)
  (setq config-dir (get-config-dir (get-config-name))
  (magit-status config-dir)))

(setq config-name (get-config-name)
      config-init-func (alist-get (intern config-name) config-init-alist))

(config-link-emacsd config-name)

(if config-init-func 
    (funcall config-init-func)
  (progn 
    (message "No config-init-func found. Using init-personal")
    (init-personal)))

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

(setq
 my-config (expand-file-name "~/.config/")
 my-emacs-config (expand-file-name "~/.config/emacs/")
 my-dotfiles (expand-file-name "~/Projects/dotfiles/")
 my-org (expand-file-name "~/org/"))

(defun is-system (system-name) (eq system-type system-name))
(defun is-mac () (is-system 'darwin))
(defun is-linux () (is-system 'gnu/linux))
(defun system-cond ()
  (cond ((is-mac) mac)
	((is-linux) linux)))
