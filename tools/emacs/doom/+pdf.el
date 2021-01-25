;;; ~/Projects/dotfiles/src/config/doom/+pdf.el -*- lexical-binding: t; -*-

;; General
;; Already migrated to new config --->
(add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)

(after! pdf-tools
  (setq-default pdf-view-display-size 'fit-width))

(map! :map pdf-view-mode-map
      :nv "C-d" #'pdf-view-next-page
      :nv "C-u" #'pdf-view-previous-page)
;; Already migrated to new config <---

;; Advice
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

(defadvice! +pdf--org-open-links (fn &rest args)
  "Open PDFs in a vertical split, open one if none-exists and
don't do anything if the buffer is already displayed."
  :around 'org-pdftools-open-pdftools
  (let ((display-buffer-overriding-action '(+buffer--display-in-vertical-split-maybe . nil)))
    (apply fn args)))

(defadvice! +pdf--resize (&rest _)
  "Center the image horizontally when resizing."
  :after '(pdf-view-enlarge
           pdf-view-shrink
           pdf-view-scale-reset)
  (+image-center-horizontally))

(defadvice! +pdf-view-next-page--top (&rest _)
  "After we move to the next page, we should start at the top of the page."
  :after '(pdf-view-next-page
           pdf-view-next-page-command)
  (image-set-window-vscroll 0))

;; Helpers
(defun +image-center-horizontally ()
  "Center an image (or PDF) horizontally."
  (interactive)
  (let* ((image (image-get-display-property))
	 (edges (window-inside-edges))
	 (pixel-edges (window-edges nil t t))
	 (win-width (- (nth 2 edges) (nth 0 edges)))
	 (img-width (ceiling (car (image-display-size image)))))
    (image-set-window-hscroll (max 0 (/ (- img-width win-width) 2)))))
