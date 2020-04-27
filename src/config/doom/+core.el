;;; ~/Projects/dotfiles/src/config/doom/+core.el -*- lexical-binding: t; -*-

(defun +ensure-buffer (name &optional setup-fn)
  (let ((buffer (get-buffer name)))
    (unless buffer
      (setq buffer (get-buffer-create name))
      (with-current-buffer buffer
          (funcall setup-fn)))
    buffer))
