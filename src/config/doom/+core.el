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
