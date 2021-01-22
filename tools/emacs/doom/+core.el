;;; ~/Projects/dotfiles/src/config/doom/+core.el -*- lexical-binding: t; -*-

(defun +buffer--display-in-vertical-split (buffer alist)
  "Uses an existing window to the right or left to display the BUFFER.
Otherwise it creates a window on the right and displays the BUFFER."
  (if-let ((side-window (or (window-in-direction 'right)
                         (window-in-direction 'left))))
        (with-selected-window side-window
          (display-buffer-same-window buffer nil))
    (display-buffer-in-direction buffer '((direction . right)))))

(defun +buffer--display-in-vertical-split-maybe (buffer alist)
  "Wraps around `+buffer--display-in-vertical-split', but only
displays the buffer if it isn't already in a window in this frame."
  (or (get-buffer-window buffer nil)
    (+buffer--display-in-vertical-split buffer alist)))

(defun +buffer-open-in-vertical-split-maybe (buffer-or-name &optional force)
  "Open the BUFFER-OR-NAME in a vertical split, but only if the buffer isn't
already displayed.

If FORCE is set, it overrides any other actions down in `display-buffer' by setting
`display-buffer-overriding-action'. For those times you really, really want things to
do as you say."
  (let ((action '(+buffer--display-in-vertical-split-maybe . nil))
        (display-buffer-overriding-action display-buffer-overriding-action))
    (when force
      (setq display-buffer-overriding-action action))
    (display-buffer buffer-or-name action)))


(defun +open-messages ()
  "Open messages buffer in vertical split, if it isn't already open."
  (interactive)
  (+buffer-open-in-vertical-split-maybe (messages-buffer) t)
  (with-current-buffer (messages-buffer)
    (set-window-point (get-buffer-window (current-buffer)) (point-max))))


(defun +ensure-buffer (name &optional setup-fn)
  (let ((buffer (get-buffer name)))
    (unless buffer
      (setq buffer (get-buffer-create name))
      (with-current-buffer buffer
          (funcall setup-fn)))
    buffer))
(defun +find-file (filename &optional wildcards)
  "Like `find-file', but with path joining.

If FILENAME is a list of file paths, then join them together.
For example:

    (+find-file '(\"/tmp/\" \"path\" \"file\"))

Will call `find-file' with the file \"/tmp/path/file\".

For WILDCARDS see `find-file'.
"
  (interactive
   (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))
  (when (listp filename)
    (setq filename (apply #'f-join filename)))
  (find-file filename wildcards))


(defun +clone-buffer (&optional newname display-flag)
  "Clones the current buffer.

If the current buffer is visiting a file, then the clone
will not be associated with the file.

This is basically a pass through to `call-buffer'."
  (interactive)
  (let* ((buffer-file-name nil)
         (buffer-file-truename nil)
         (cloned-buffer (if (called-interactively-p)
                            (call-interactively #'clone-buffer)
                          (clone-buffer newname display-flag))))
    (with-current-buffer cloned-buffer
      (set-buffer-modified-p t))
    cloned-buffer))

(defun +rename-buffer (name &optional unique)
  "An interactive version of `rename-buffer'.

See `rename-buffer' for valid values of NAME and UNIQUE."
    (interactive
     (list (read-string (format "Rename '%s' to: " (buffer-name)))))
  (when (null name)
    (user-error "No name provided, buffer name unchanged"))
  (rename-buffer name unique))


(after! ivy
  (defun +counsel/popup-buffer ()
    "Pull a buffer in as a popup.

TODO: Add some extra actions to specifiy where to pull the popup at
"
    (interactive)
    (ivy-read "Popup buffer: " #'internal-complete-buffer
              :preselect (buffer-name (other-buffer (current-buffer)))
              :action #'(lambda (match)
                          (+popup-buffer (get-buffer match)))
              :matcher #'ivy--switch-buffer-matcher
              :caller 'ivy-switch-buffer ;; setting to get prettyness from 'ivy-rich
              ))

  (ivy-configure '+counsel/popup-buffer
    :display-transformer-fn #'ivy-switch-buffer-transformer))


(defun +set-buffer-local-variable ()
  (interactive)
  (ivy-read "[buffer] Change variable: " (buffer-local-variables)
            :require-match t
            :action #'(lambda (var-record)
                        (when var-record
                          (eval `(setq-local ,(car var-record)
                                             ,(read (read-string
                                                     "elisp value: "))))))
            :caller 'counsel-describe-variable))

(defun +counsel-describe-buffer-local-variable ()
  (interactive)
  (ivy-read "[buffer] Describe variable: " (buffer-local-variables)
            :require-match t
            :keymap counsel-describe-map
            :action (lambda (x)
                      (funcall counsel-describe-variable-function (intern x)))
            :caller 'counsel-describe-variable))


(defun apply-partially-r (fn &rest args)
  (-rpartial fn args))

(defun apply-nth (fn n arg)
  (lambda (&rest args)
    (apply fn
           (append (subseq args 0 n)
                   (list arg)
                   (subseq args n)))))

(defun enlist (exp)
  (if (listp exp) exp (list exp)))

(defun apply-enlist (fn args)
  (apply fn (enlist args)))

(defun tee (args &rest fns)
  (mapcar
   (apply-partially-r #'apply-enlist args)
   fns))

(defun pipe (args &rest fns)
  (apply-enlist
   (apply #'-compose fns)
   args))

(defun d (command)
  (shell-command (concat "d " command)))

(defun firefox (url &optional container)
  (let ((url (if container
                 (format "ext+container:name=%s&url=%s"
                         container
                         (url-hexify-string url))
                 url)))
    (when url
      (d (format "firefox '%s'" url)))))

(defmacro with-directory (directory &rest body)
  `(let ((default-directory ,directory))
    ,@body))

(defmacro with-contents (contents &rest body)
  `(with-temp-buffer
     (insert ,contents)
     ,@body))

(defun trim-trailing-newline (string)
  (with-contents string
                 (goto-char (point-max))
                 (when (= (char-before) ?\n)
                   (delete-char -1))
                 (buffer-string)))
