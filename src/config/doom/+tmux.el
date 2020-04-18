;;; ~/Projects/dotfiles/src/config/doom/+tmux.el -*- lexical-binding: t; -*-

(defun +tmux--find-by-prop (items prop val)
  (let (matched-item)
    (dolist (item items matched-item)
      (if (equal
           (plist-get (cdr item) prop) ;; first element is an id, need to pop it off to get the following plist
           val)
          (setq matched-item item)))))

(defun +tmux-get-session (session-name)
  (+tmux--find-by-prop (+tmux-list-sessions) :name session-name))

(defun +tmux-get-window (session window-name)
  (+tmux--find-by-prop (+tmux-list-windows session)  :name window-name))

(defun +tmux-get-pane (window pane-name)
  (+tmux--find-by-prop (+tmux-list-panes window) :name pane-name))


(defun +tmux--generate-valid-session-name (session-name)
  (replace-regexp-in-string "[:.]" "-" session-name))

(defun +tmux-workspace-get-session-name ()
  (let* ((workspace-name (if (bound-and-true-p persp-mode)
                             (safe-persp-name (get-current-persp))
                           "main"))
         (scrubbed-workspace-name (+tmux--generate-valid-session-name workspace-name)))
    (format "[doom] %s" scrubbed-workspace-name)))

(defun +tmux-workspace-get-or-create-session ()
  (+tmux-get-or-create-session (+tmux-workspace-get-session-name)))

(defun +tmux-workspace-list-windows ()
  (+tmux-list-windows (+tmux-get-workspace-session)))

(defun +tmux-workspace-get-window (window-name)
  (+tmux-get-window (+tmux-get-workspace-session) window-name))

(defun +tmux-workspace-list-panes ()
  (+tmux-list-windows (+tmux-get-workspace-session)))

(defun +tmux-session-p (tmux-obj)
  (string-prefix-p "$" (car tmux-obj)))

(defun +tmux-window-p (tmux-obj)
  (string-prefix-p "@" (car tmux-obj)))

(defun +tmux-pane-p (tmux-obj)
  (string-prefix-p "#" (car tmux-obj)))

(defun +tmux--generate-command-target (tmux-obj)
  (if tmux-obj
      (format " -t '%s'" (car tmux-obj))
    ""))

(defun +tmux-send-keys (keys tmux-obj)
  "Run COMMAND in tmux. If NORETURN is non-nil, send the commands as keypresses
but do not execute them."
  (+tmux (format "send-keys %s %s"
                 (+tmux--generate-command-target tmux-obj)
                 keys)))

(defun +tmux-run-in (command &optional tmux-obj)
  "Run COMMAND in tmux. If NORETURN is non-nil, send the commands as keypresses
but do not execute them."
  (interactive
   (list (read-string "tmux $ ")))
  (+tmux-send-keys (format "C-u %s Enter" (shell-quote-argument command)) tmux-obj))

(defun +tmux-ctrl-c (tmux-obj)
  (+tmux-send-keys "C-c" tmux-obj))

(defun +tmux-create-session (session-name)
  (+tmux (format "new-session -s '%s' -d" session-name))
  (+tmux-get-session session-name))

(defun +tmux-get-or-create-session (session-name)
  (let ((session (+tmux-get-session session-name)))
    (unless session
      (setq session (+tmux-create-session session-name)))
    session))

(defun +tmux-create-window (session window-name)
  (+tmux (format "new-window %s -n '%s' -d" (+tmux--generate-command-target session) window-name))
  (+tmux-get-window session window-name))

(defun +tmux-get-or-create-window (session window-name)
  (let ((window (+tmux-get-window session window-name)))
    (unless window
      (setq window (+tmux-create-window session window-name)))
    window))

(defun +tmux-kill-session (session-name)
  (let ((session (+tmux-get-session session-name)))
    (if session
        (let ((confirm (read-string (format "Kill '%s' tmux session? " session-name))))
          (when (string-prefix-p "y" confirm t)
            (+tmux (format "kill-session %s" (+tmux--generate-command-target session)))))
        (error "Session '%s' does not exist." session-name))))


(defun +tmux-workspace-run (command)
  (+tmux-run-in command (+tmux-workspace-get-or-create-session)))
