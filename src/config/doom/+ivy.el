;;; ~/Projects/dotfiles/src/config/doom/+ivy.el -*- lexical-binding: t; -*-

(after! ivy
  (setq ivy-read-action-function #'ivy-hydra-read-action)
  (push '(swiper . 10) ivy-height-alist))

(defadvice! +ivy--compute-extra-actions-dedup-a (results)
  "Removes duplicates from `ivy--compute-extra-actions'. This fixes a 'bug' (?)
where there are duplicate actions with the same key."
  :filter-return #'ivy--compute-extra-actions
  (setcdr results
          (cl-delete-duplicates (cdr results)
                         :key #'car :test #'equal
                         :from-end t))
  results)

(defadvice! +counsel-projectile-switch-project--set-default-a (args)
  :filter-args #'counsel-projectile-switch-project
  (list (or
         (car args)
         "v")))
