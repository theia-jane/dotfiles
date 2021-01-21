;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

(defmacro packages! (&rest package-defs)
  `(progn ,@(mapcar
             (lambda (package-def)
               `(package! ,@(if (listp package-def)
                                package-def
                              (list package-def))))
             package-defs)))

(packages!
 ;; Don't think I need these two any more:
 all-the-icons-ivy
 all-the-icons-dired

 ;; I like to try things
 try
 command-log-mode

 ;; Math stuff
 lean-mode
 company-lean
 ob-sagemath

 ;; graphql
 graphql-mode
 (ob-graphql
  :recipe (:host github :repo "tylerware/ob-graphql"))

 ;; Org
 (ob-async
  :recipe (:host github :repo "tylerware/ob-async"))
 doct
 (org-transclusion
  :recipe (:host github :repo "nobiot/org-transclusion"))

 ;; misc
 ;; Package I made.. not sure if it's worth anything
 (rextract
  :recipe (:host github :repo "tylerware/rextract.el"))
 vimrc-mode
 nov
 (ox-ssh
  :recipe (:host github :repo "tylerware/ox-ssh"))

 ;; Unpin
 (ivy :pin nil)
 (swiper :pin nil)

 ;; Disabled packages
 (hl-line :disable t)
 (saveplace :disable t)
 (solaire-mode :disable t))
