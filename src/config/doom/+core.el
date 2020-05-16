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

(defun +ensure-buffer (name &optional setup-fn)
  (let ((buffer (get-buffer name)))
    (unless buffer
      (setq buffer (get-buffer-create name))
      (with-current-buffer buffer
          (funcall setup-fn)))
    buffer))
