;;; ~/Projects/dotfiles/src/config/doom/+bookmark.el -*- lexical-binding: t; -*-


(after! org
  (defvar +bookmark-org-store-link-modes nil "")

  (defun +bookmark-org--get-record (bookmark-record-or-name)
    (if (stringp bookmark-record-or-name)
        (bookmark-get-bookmark bookmark-record-or-name)
      bookmark-record-or-name))

  (defun +bookmark-org--generate-path (bookmark-record-or-name)
    ""
    (let* ((bookmark-record (bookmark-get-bookmark bookmark-record-or-name)))
      (when bookmark-record
        ;; Stole this from (pp-to-string), basicallly, don't pretty print
        (let ((print-escape-newlines t)
              (print-quoted t))
          (base64-encode-string (prin1-to-string bookmark-record) t))))))
 
  (defun +bookmark-org--generate-link (bookmark-record-or-name)
    (concat "bookmark:"
            (+bookmark-org--generate-path bookmark-record-or-name)))

  (defun +bookmark-org-store-link ()
    "Store a `bookmark:' link. "
    (require 'bookmark)
    (let ((bookmark-record (bookmark-make-record)))
      (when (or (and (null (buffer-file-name))
                     (not (eq bookmark-make-record-function 'bookmark-make-record-default)))
                (memq major-mode +bookmark-org-store-link-modes))
        (org-link-store-props
         :type "bookmark"
         :link (+bookmark-org--generate-link bookmark-record)
         :description (bookmark-name-from-full-record bookmark-record)))))

  (defun +bookmark-org-follow-link (path _)
    "Follow a `bookmark' link."
    (let* ((path-read (read (base64-decode-string path)))
           (bookmark-or-name (if (listp path-read) path-read path)))
      (bookmark-jump bookmark-or-name)))

  (defun +bookmark-org-complete-link ()
    (let* ((enable-recursive-minibuffers t))
      (+bookmark-org--generate-link
       (ivy-read "Bookmark: "
                 (bookmark-all-names)))))

  (org-link-set-parameters "bookmark"
                           :complete #'+bookmark-org-complete-link
                           :follow #'+bookmark-org-follow-link
                           :store #'+bookmark-org-store-link))
