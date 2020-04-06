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
   :desc "Toggle Highlight" "t h" 'evil-ex-nohighlight)
 ;; Global bindings
 (:desc "Jump forward" "C->" 'better-jumper-jump-forward
   :desc "Jump back" "C-<" 'better-jumper-jump-backward))
