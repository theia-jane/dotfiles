;;; ../../Projects/dotfiles/src/config/doom/+snippets.el -*- lexical-binding: t; -*-

(defun snippets-docs ()
  ""
  (interactive)
  (find-file
   (concat
    (file-name-directory
     (file-truename
      (locate-file "yasnippet.el" load-path)))
    "doc/index.org")))


(map! :leader
  (:prefix ("S" . "snippets")
   :desc "New snippet" "n" #'yas-new-snippet ;; #'+snippets/new
   :desc "Documentation" "d" #'snippets-docs
   :desc "Edit snippet" "e" #'+snippets/edit
   :desc "Make alias snippet" "a" #'+snippets/new-alias
   (:prefix ("f" . "find")
    :desc "Find" "f" #'+snippets/find-for-current-mode
    :desc "Find mine" "m" #'+snippets/find-private
    :desc "Find all" "a" #'+snippets/find)))
