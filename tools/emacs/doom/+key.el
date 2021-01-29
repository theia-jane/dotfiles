;;; ~/Projects/dotfiles/src/config/doom/+key.el -*- lexical-binding: t; -*-

(define-leader! personal "s-,")
(keymap! :personal
         (:prefix ("o" . "open")
          :desc "Fruit basket" "f" (cmd! (find-file (expand-file-name "projects/fruit-basket.org" org-directory))))
         (:prefix ("f" . "files")
          :desc "Find in config" "p" (cmd! (doom-project-find-file personal/config-directory))
          :desc "Find in homework" "h" (cmd! (doom-project-find-file notes/homework-directory)))
         (:prefix ("c" . "Config")
          :desc "Open config" "c" (cmd! (let ((default-directory personal/config-directory))
                                          (magit)))
          :desc "Tangle config" "t" (cmd! (d "config-tangle")))
         (:prefix ("p" . "Project")
          :desc "Open config" "c" (cmd! (let ((default-directory personal/config-directory))
                                          (magit)))))
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
 (:prefix ("o" . "Open")
  :desc "Treemacs" "t" #'treemacs
  :desc "Re-Builder" "b" #'re-builder)))
