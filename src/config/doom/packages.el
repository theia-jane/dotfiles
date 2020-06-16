;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;; Don't think I need these two any more:
(package! all-the-icons-ivy)
(package! all-the-icons-dired)


;; I like to try things
(package! try)
(package! command-log-mode)
(package! ob-sagemath)

;; A curiosity
(package! howdoyou)

;; Package I made.. not sure if it's worth anything
(package! rextract
  :recipe (:host github :repo "tylerware/rextract.el"))

(package! graphql-mode)
(package! ob-graphql)

(package! doct)
(package! hl-line :disable t)

(unpin! ivy)
(unpin! swiper)
