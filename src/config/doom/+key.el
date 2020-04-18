;;; ~/Projects/dotfiles/src/config/doom/+key.el -*- lexical-binding: t; -*-

(map!
 ;; Normal mode
 (:map evil-normal-state-map
   :desc "Open file at point" "g f" (lambda () (interactive)
                                      (let ((file (ffap-guess-file-name-at-point)))
                                        (when file
                                          (find-file file)))))
 ;; Leader bindings
 (:leader
   :desc "Toggle Highlight" "t h" 'evil-ex-nohighlight
   (:prefix ("S" . "snippets")
     :desc "New snippet"          "n" #'yas-new-snippet ;; #'+snippets/new
     :desc "Edit snippet"         "e" #'+snippets/edit
     :desc "Make alias snippet"   "a" #'+snippets/new-alias
     (:prefix ("f" . "find")
       :desc "Find"               "f" #'+snippets/find-for-current-mode
       :desc "Find mine"          "m" #'+snippets/find-private
       :desc "Find all"           "a" #'+snippets/find
       ))
   (:prefix ("P" . "personal")
     (:prefix ("o" . "open")

       )
     (:prefix ("n" . "new")
       :desc "Homework"           "h" (lambda () (interactive)
                                        (let* ((homework-buffer (generate-new-buffer "homework")))
                                          (switch-to-buffer homework-buffer)
                                          (cd (expand-file-name "~/homework"))
                                          (org-mode)
                                          (insert "__hw")
                                          (yas-expand-from-trigger-key)))
       ))
   )
 ;; Global bindings
 (:desc "Jump forward" "C->" 'better-jumper-jump-forward
   :desc "Jump back" "C-<" 'better-jumper-jump-backward))
