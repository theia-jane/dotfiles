;;; pretty-todo.el --- Replaces org todo keywords with icons -*- lexical-binding: t; -*-

(defun map-alist (f alist)
  (mapcar (lambda (key-val)
            (setq key (car key-val)
                  val (cdr key-val))
            (funcall f key val))
          alist))

(defun org-mode-todo-symbols (todo-alist)
  (setq org-todo-font-lock-replace
        (map-alist (lambda (keyword symbol)
                     `(,(concat "^\\*+ \\(" keyword "\\) ")
                       (1 (progn (compose-region (match-beginning 1) (match-end 1) ,symbol) nil))))
                   todo-alist))

  (font-lock-add-keywords
   'org-mode org-todo-font-lock-replace))

(org-mode-todo-symbols
 '(("TODO" . "⚑")
   ("STARTED" .  "⚐")
   ("CANCELED" .  "✘")
   ("DONE" .  "✔")))

(provide 'pretty-todo)
