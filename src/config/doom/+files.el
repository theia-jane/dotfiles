;;; ~/Projects/dotfiles/src/config/doom/+files.el -*- lexical-binding: t; -*-

(defadvice! +doom/copy-this-file--visit-file (fn path &optional force-p)
  "Visit the file after copying it."
  :around 'doom/copy-this-file
  (funcall fn path force-p)
  (when (file-exists-p path)
    (find-file path)))
