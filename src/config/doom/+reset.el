;;; ~/Projects/dotfiles/src/config/doom/+reset.el -*- lexical-binding: t; -*-


(undefine-key! global-map
      "<C-return>"
      "<C-S-return>")

(undefine-key! evil-normal-state-map
      "<C-return>"
      "<C-S-return>")

(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; (undefine-key! global-map
;;   "C-s"
;;   "C-h"
;;   "C-x")

;; (defun +use-key-message (&rest keys)
;;   (message "%s" (type-of (car keys)))
;;   `(lambda () (interactive)
;;      (message "Use %s key instead."
;;               (string-join ',keys " "))))


;; (map! :map global-map
;;  "C-h" `,(+use-key-message doom-leader-key "h")
;;  "C-s" `,(+use-key-message "/"))
