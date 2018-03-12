
(setq exec-path-from-shell-check-startup-files nil)
(defun source-file-and-get-envs (filename)
  (let* ((cmd (concat ". " filename "; env"))
         (env-str (shell-command-to-string cmd))
         (env-lines (split-string env-str "\n"))
         (envs (mapcar (lambda (s) (replace-regexp-in-string "=.*$" "" s)) env-lines)))
    (delete "" envs)))

(exec-path-from-shell-copy-envs (source-file-and-get-envs "~/.profile"))

;;    (define-key evil-insert-state-map (kbd "C-.") "hello")

;;(setq yas-snippet-dirs
 ;;  '(
  ;;   "~/.config/personal/snippets"
   ;;))

;;(setq backup-by-copying-when-linked t)

(setq mode-require-final-newline nil)

;(defun append-to-list (list-var elements)
     ;"Append ELEMENTS to the end of LIST-VAR.
;
     ;The return value is the new value of LIST-VAR."
       ;(unless (consp elements)
         ;(error "ELEMENTS must be a list"))
       ;(let ((list (symbol-value list-var)))
         ;(if list
             ;(setcdr (last list) elements)
           ;(set list-var elements)))
       ;(symbol-value list-var))

;; (slack-register-team
;;  :name ""
;;  :default t
;;  :client-id ""
;;  :client-secret ""
;;  :token ""
;;  )

(setq org-log-into-drawer "logbook")
(setq org-agenda-files '("~/org/tsheets"))
(setq org-directory "~/org")
(setq org-modules (append org-modules '(org-drill)))

(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "|" "DONE" "CANCELED")))

(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (emacs-lisp . t)
   (js . t)
   (latex . t)
   ;; (php . t)
   (dot . t)
   (shell . t)))

(setq org-capture-templates '(
                              ("p" "Plain" entry (file "")
                               "* %?")
                              ("t" "Todo" entry (file "")
                               "* TODO %?")
                              ))

(setq org-mobile-inbox-for-pull "~/Nextcloud/org/flagged.org")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

(setq org-refile-targets '((nil :maxlevel . 7)
                          (org-agenda-files :maxlevel . 5)))
(setq org-refile-allow-creating-parent-nodes t)
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)                  ; Show full paths for refiling

;(setq ns-pop-up-frames nil)
;(setq ns-use-srgb-colorspace nil)
;(setq ns-pop-up-frames nil)

(setq powerline-default-separator 'slant)

(setq evil-overriding-maps nil)
(setq evil-intercept-maps nil)
