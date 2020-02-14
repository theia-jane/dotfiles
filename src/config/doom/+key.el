;;; ~/Projects/dotfiles/src/config/doom/+key.el -*- lexical-binding: t; -*-


(general-define-key
 :keymaps 'evil-normal-state-map
 "g f" (lambda () (interactive)
         (let ((file (ffap-guess-file-name-at-point)))
           (when file
             (find-file file))))
 )

(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "t h" 'evil-ex-nohighlight
 )

(general-define-key
 :keymaps 'global-map
 "C->" 'better-jumper-jump-forward
 "C-<" 'better-jumper-jump-backward
 )
