;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;; Don't think I need these two any more:
(package! all-the-icons-ivy)
(package! all-the-icons-dired)


;; I like to try things
(package! try)
(package! command-log-mode)
(package! ob-sagemath)

;; Package I made.. not sure if it's worth anything
(package! rextract
  :recipe (:host github :repo "tylerware/rextract.el"))

(package! graphql-mode)
(package! ob-graphql
  :recipe (:host github :repo "tylerware/ob-graphql"))
(package! ob-async
  :recipe (:host github :repo "tylerware/ob-async"))

;; declarative org capture templates
(package! doct)

;; narrowing dired buffers
(package! dired-narrow)
(package! hl-line :disable t)
(package! saveplace :disable t)
(package! solaire-mode :disable t)

(package! vimrc-mode)
(package! ox-ssh
  :recipe (:host github :repo "tylerware/ox-ssh"))
(package! nov)

(unpin! ivy)
(unpin! swiper)

(package! org-transclusion
  :recipe (:host github :repo "nobiot/org-transclusion"))
