;;; ~/Projects/dotfiles/src/config/doom/+helpful.el -*- lexical-binding: t; -*-

(defun +helpful-org-store-link ()
  "Store a `helpful:' link.

This might not behave as expected if you are in a helpful
buffer that is for a local variable / function."
  (when (and (eq major-mode 'helpful-mode)
             'helpful--sym)
    (org-link-store-props
     :type "helpful"
     :link (concat "helpful:" (symbol-name helpful--sym))
     :description (symbol-name helpful--sym))))


(defun +helpful-org-follow-link (path _)
  "Follow a `helpful' link."
  (helpful-symbol (intern path)))

(defun counsel-helpful-symbol ()
  "Forward to `helpful-symbol'."
  (interactive)
  (let ((enable-recursive-minibuffers t))
    (ivy-read "Describe symbol: " obarray
              :predicate (lambda (sym) (or (fboundp sym)
                                      (and (boundp sym)
                                           (not (keywordp sym)))
                                      (get sym 'variable-documentation)
                                      (get sym 'function-documentation)))
              :require-match t
              ;; :preselect (funcall counsel-describe-function-preselect)
              :caller 'counsel-helpful-symbol)))

(defun +helpful-org-complete-link (&optional arg)
  "TODO"
  (concat "helpful:" (or (counsel-helpful-symbol) "")))

(org-link-set-parameters "helpful"
                         :complete #'+helpful-org-complete-link
                         :follow #'+helpful-org-follow-link
                         :store #'+helpful-org-store-link)
