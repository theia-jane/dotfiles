;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! all-the-icons-ivy)
(package! all-the-icons-dired)
(package! try)
(package! command-log-mode)
(package! ob-sagemath)
(package! howdoyou)
(package! rextract
  :recipe (:host github :repo "tylerware/rextract.el"))

(package! doct)
(package! hl-line :disable t)
