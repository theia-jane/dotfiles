;;; ~/Projects/dotfiles/src/config/doom/+ivy.el -*- lexical-binding: t; -*-

(after! ivy
  (setq ivy-read-action-function #'ivy-hydra-read-action
        ivy-height 13)
  (push '(swiper . 10) ivy-height-alist))


(defadvice! +counsel-projectile-switch-project--set-default-a (args)
  :filter-args #'counsel-projectile-switch-project
  (list (or (car args)
            #'counsel-projectile-switch-project-action-vc)))
