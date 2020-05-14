;;; ~/Projects/dotfiles/src/config/doom/+pdf.el -*- lexical-binding: t; -*-
;;;

(add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)

(defadvice! +pdf-view--preserve-hscroll (fn &rest args)
  "Temporary hack to resolve issue in pdf tools and evil collection.

Fixing the issue in pdf-view will involve changing the usages of
`set-window-hscroll' to `image-set-window-hscroll', which will make sure the
buffer hscroll state gets restored.

Fixing evil collection will basically involve copying and pasting the code below.
Those methods call `bob' and `eob', which adjust hscroll"
  :around '(pdf-view-scroll-up-or-next-page
            pdf-view-scroll-down-or-previous-page
            pdf-view-next-line-or-next-page
            pdf-view-previous-line-or-previous-page
            evil-collection-pdf-view-goto-first-page
            evil-collection-pdf-view-goto-page)
  (let ((hscroll (window-hscroll)))
    (apply fn args)
    (image-set-window-hscroll hscroll)))
