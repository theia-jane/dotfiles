;;; ~/Projects/dotfiles/src/config/doom/+key.el -*- lexical-binding: t; -*-

(define-leader! personal "s-,")

(map!
 ;; Normal mode
 (:map evil-normal-state-map
  :desc "Open file at point" "g f" (lambda () (interactive)
                                     (let ((file (ffap-guess-file-name-at-point)))
                                       (when file
                                         (find-file file)))))
 (:map (reb-mode-map reb-lisp-mode-map)
  :nv "q" #'reb-quit
  :localleader
  :nv "RET" #'reb-force-update
  :nv "q" #'reb-quit
  :ni "j" #'reb-next-match
  :ni "k" #'reb-prev-match
  :n "TAB" #'reb-change-syntax
  :n "b" #'reb-change-target-buffer
  :n "y" #'reb-copy)
 (:map ivy-minibuffer-map
  "M-SPC" #'ivy-mark)
 ;; Leader bindings
 (:leader
  :desc "Toggle Highlight" "t h" #'evil-ex-nohighlight
  :desc "Diff file" "g d" #'+magit-review-this-file
  :desc "Messages" "h C-m" #'+open-messages
  (:prefix ("S" . "snippets")
   :desc "New snippet" "n" #'yas-new-snippet ;; #'+snippets/new
   :desc "Edit snippet" "e" #'+snippets/edit
   :desc "Make alias snippet" "a" #'+snippets/new-alias
   (:prefix ("f" . "find")
    :desc "Find" "f" #'+snippets/find-for-current-mode
    :desc "Find mine" "m" #'+snippets/find-private
    :desc "Find all" "a" #'+snippets/find))
  (:prefix ("b" . "Buffer")
    :desc "Erase buffer" "e" #'erase-buffer
    :desc "Rename buffer" "R" #'+rename-buffer
    :desc "Clone buffer" "C" #'+clone-buffer)
  :desc "How do you..." "s h" #'howdoyou-query
  :desc "Re-Builder" "o b" #'re-builder)

 ;; Global bindings
 (:desc "Jump forward" "C->" 'better-jumper-jump-forward
  :desc "Jump back" "C-<" 'better-jumper-jump-backward
  :vimerong "M-m" (eval `(general-simulate-key ,doom-localleader-key  :state 'normal)))

 (:personal-leader
  (:prefix ("o" . "open")
   :desc "Fruit basket" "f" (cmd! (+find-file `(,org-directory "projects" "fruit-basket.org"))))
  (:prefix ("f" . "files")
   :desc "Find in private config" "p" (cmd! (doom-project-find-file personal/config-directory)))
  (:prefix ("n" . "new")
    :desc "Homework" "h" (lambda () (interactive)
                                     (let* ((homework-buffer (generate-new-buffer "homework")))
                                       (switch-to-buffer homework-buffer)
                                       (cd (expand-file-name "~/homework"))
                                       (org-mode)
                                       (insert "__hw")
                                       (yas-expand-from-trigger-key))))))
