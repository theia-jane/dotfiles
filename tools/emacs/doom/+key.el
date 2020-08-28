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
  (:prefix ("t" . "Toggle")
   :desc "Highlight" "h" #'evil-ex-nohighlight
   :desc "Modeline" "m" #'doom-modeline-mode
   :desc "Present" "p" #'(lambda () (interactive)
                             (let ((not-presenting? (or doom-modeline-mode
                                                        (not doom-big-font-mode)
                                                        (not hl-line-mode)
                                                        global-company-mode)))
                               (doom-modeline-mode (when not-presenting? -1))
                               (doom-big-font-mode (unless not-presenting? -1))
                               (hl-line-mode (unless not-presenting? -1))
                               (global-company-mode (when not-presenting? -1))))
   :desc "Org Mode" "o" #'(lambda () (interactive)
                              (if (eq major-mode 'org-mode) (fundamental-mode) (org-mode))))
 :desc "Diff file" "g d" #'+magit-review-this-file
 :desc "Messages" "h e" #'+open-messages
 (:prefix ("b" . "Buffer")
  :desc "Erase buffer" "e" #'erase-buffer
  :desc "Rename buffer" "R" #'+rename-buffer
  :desc "Clone buffer" "C" #'+clone-buffer)
 :desc "How do you..." "s h" #'howdoyou-query
 (:prefix ("o" . "Open")
  :desc "Treemacs" "t" #'treemacs
  :desc "Re-Builder" "b" #'re-builder))

 ;; Global bindings
 (:desc "Jump forward" "C->" 'better-jumper-jump-forward
  :desc "Jump back" "C-<" 'better-jumper-jump-backward
  :vimerong "M-m" (eval `(general-simulate-key ,doom-localleader-key  :state 'normal)))

 (:personal-leader
  (:prefix ("o" . "open")
   :desc "Fruit basket" "f" (cmd! (+find-file `(,org-directory "projects" "fruit-basket.org"))))
  (:prefix ("f" . "files")
   :desc "Find in config" "p" (cmd! (doom-project-find-file personal/config-directory))
   :desc "Find in homework" "h" (cmd! (doom-project-find-file notes/homework-directory)))
  (:prefix ("c" . "Config")
   :desc "Open config" "c" (cmd! (let ((default-directory personal/config-directory))
                                   (magit)))
   :desc "Tangle config" "t" (cmd! (d "config-tangle")))
  (:prefix ("p" . "Project")
   :desc "Open config" "c" (cmd! (let ((default-directory personal/config-directory))
                                   (magit))))))
