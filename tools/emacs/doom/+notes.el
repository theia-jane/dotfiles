;;; ../../Projects/dotfiles/src/config/doom/+notes.el -*- lexical-binding: t; -*-

(use-package! doct
  :commands (doct))

(after! org-capture
  (setf (alist-get 'height +org-capture-frame-parameters) 15)
  (setq +org-capture-fn
        (lambda ()
          (interactive)
          (set-window-parameter nil 'mode-line-format 'none)
          (org-capture)))

  (defun org-capture-select-template-prettier (&optional keys)
    "Select a capture template, in a prettier way than default
Lisp programs can force the template by setting KEYS to a string."
    (let ((org-capture-templates
           (or (org-contextualize-keys
                (org-capture-upgrade-templates org-capture-templates)
                org-capture-templates-contexts)
               '(("t" "Task" entry (file+headline "" "Tasks")
                  "* TODO %?\n  %u\n  %a")))))
      (if keys
          (or (assoc keys org-capture-templates)
              (error "No capture template referred to by \"%s\" keys" keys))
        (org-mks org-capture-templates
                 "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
                 "Template key: "
                 `(("q" ,(concat (all-the-icons-octicon "stop" :face 'all-the-icons-red :v-adjust 0.01) "\tAbort")))))))
  (advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier)

  (defun org-mks-pretty (table title &optional prompt specials)
    "Select a member of an alist with multiple keys. Prettified.

TABLE is the alist which should contain entries where the car is a string.
There should be two types of entries.

1. prefix descriptions like (\"a\" \"Description\")
   This indicates that `a' is a prefix key for multi-letter selection, and
   that there are entries following with keys like \"ab\", \"ax\"…

2. Select-able members must have more than two elements, with the first
   being the string of keys that lead to selecting it, and the second a
   short description string of the item.

The command will then make a temporary buffer listing all entries
that can be selected with a single key, and all the single key
prefixes.  When you press the key for a single-letter entry, it is selected.
When you press a prefix key, the commands (and maybe further prefixes)
under this key will be shown and offered for selection.

TITLE will be placed over the selection in the temporary buffer,
PROMPT will be used when prompting for a key.  SPECIALS is an
alist with (\"key\" \"description\") entries.  When one of these
is selected, only the bare key is returned."
    (save-window-excursion
      (let ((inhibit-quit t)
            (buffer (org-switch-to-buffer-other-window "*Org Select*"))
            (prompt (or prompt "Select: "))
            case-fold-search
            current)
        (unwind-protect
            (catch 'exit
              (while t
                (setq-local evil-normal-state-cursor (list nil))
                (erase-buffer)
                (insert title "\n\n")
                (let ((des-keys nil)
                      (allowed-keys '("\C-g"))
                      (tab-alternatives '("\s" "\t" "\r"))
                      (cursor-type nil))
                  ;; Populate allowed keys and descriptions keys
                  ;; available with CURRENT selector.
                  (let ((re (format "\\`%s\\(.\\)\\'"
                                    (if current (regexp-quote current) "")))
                        (prefix (if current (concat current " ") "")))
                    (dolist (entry table)
                      (pcase entry
                        ;; Description.
                        (`(,(and key (pred (string-match re))) ,desc)
                         (let ((k (match-string 1 key)))
                           (push k des-keys)
                           ;; Keys ending in tab, space or RET are equivalent.
                           (if (member k tab-alternatives)
                               (push "\t" allowed-keys)
                             (push k allowed-keys))
                           (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) (propertize "›" 'face 'font-lock-comment-face) "  " desc "…" "\n")))
                        ;; Usable entry.
                        (`(,(and key (pred (string-match re))) ,desc . ,_)
                         (let ((k (match-string 1 key)))
                           (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) "   " desc "\n")
                           (push k allowed-keys)))
                        (_ nil))))
                  ;; Insert special entries, if any.
                  (when specials
                    (insert "─────────────────────────\n")
                    (pcase-dolist (`(,key ,description) specials)
                      (insert (format "%s   %s\n" (propertize key 'face '(bold all-the-icons-red)) description))
                      (push key allowed-keys)))
                  ;; Display UI and let user select an entry or
                  ;; a sub-level prefix.
                  (goto-char (point-min))
                  (unless (pos-visible-in-window-p (point-max))
                    (org-fit-window-to-buffer))
                  (let ((pressed (org--mks-read-key allowed-keys prompt)))
                    (setq current (concat current pressed))
                    (cond
                     ((equal pressed "\C-g") (user-error "Abort"))
                     ;; Selection is a prefix: open a new menu.
                     ((member pressed des-keys))
                     ;; Selection matches an association: return it.
                     ((let ((entry (assoc current table)))
                        (and entry (throw 'exit entry))))
                     ;; Selection matches a special entry: return the
                     ;; selection prefix.
                     ((assoc current specials) (throw 'exit current))
                     (t (error "No entry available")))))))
          (when buffer (kill-buffer buffer))))))
  (advice-add 'org-mks :override #'org-mks-pretty)

  (defun +doct-icon-declaration-to-icon (declaration)
    "Convert :icon declaration to icon"
    (let ((name (pop declaration))
          (set  (intern (concat "all-the-icons-" (plist-get declaration :set))))
          (face (intern (concat "all-the-icons-" (plist-get declaration :color))))
          (v-adjust (or (plist-get declaration :v-adjust) 0.01)))
      (apply set `(,name :face ,face :v-adjust ,v-adjust))))

  (defun +doct-iconify-capture-templates (groups)
    "Add declaration's :icon to each template group in GROUPS."
    (let ((templates (doct-flatten-lists-in groups)))
      (setq doct-templates (mapcar (lambda (template)
                                     (when-let* ((props (nthcdr (if (= (length template) 4) 2 5) template))
                                                 (spec (plist-get (plist-get props :doct) :icon)))
                                       (setf (nth 1 template) (concat (+doct-icon-declaration-to-icon spec)
                                                                      "\t"
                                                                      (nth 1 template))))
                                     template)
                                   templates))))

  (setq doct-after-conversion-functions '(+doct-iconify-capture-templates))

  (mapcar #'car org-capture-templates)

  (defun add-doct! (declarations)
    (setq org-capture-templates (cl-delete-duplicates
                                 (append org-capture-templates
                                         (doct declarations))
                                 :key #'car
                                 :test #'equal)))

  (setq org-capture-templates nil)
  ;; add-transient-hook! 'org-capture-select-template
  (add-doct! `(("Shower thought" :keys "s"
                :icon ("light-bulb" :set "octicon" :color "yellow")
                :file "projects/shower-thoughts.org"
                :prepend t
                :type entry
                :template "* %?")
               ("Fruit basket snack" :keys "f"
                :icon ("shopping-basket" :set "faicon" :color "red")
                :file "projects/fruit-basket.org"
                :prepend t
                :type entry
                :template "* TODO %?")
               ("Is something broken?" :keys "b"
                :icon ("bug" :set "faicon" :color "green")
                :file "projects/broken-windows.org"
                :prepend t
                :type entry
                :template "* TODO %?")
               ("I probably won't read it, but I'll store of anyway..." :keys "r"
                :icon ("book" :set "faicon" :color "blue")
                :file "projects/reading.org"
                :prepend t
                :type entry
                :template ("* TODO %?" "%t"))
               ("File it later" :keys "l"
                :icon ("desktop_windows" :set "material" :color "orange")
                :file "needs-filing.org"
                :type entry
                :template ("* %?"))
               ("Personal" :keys "p"
                :icon ("person" :set "material" :color "green")
                :children (("Month note" :keys "m"
                            :icon ("calendar" :set "octicon" :color "orange")
                            :prepend t
                            :file ,(notes/month-file)
                            :type entry
                            :template "* %?")))
               )))

;;; Keymap
(map! :personal-leader
      (:prefix ("n" . "new")
       :desc "Homework" "h" #'+notes/homework
       :desc "Monthly notes" "m" #'+notes/month))

;;; Functions
(defun +notes/homework ()
    (interactive)
  (let* ((homework-buffer (generate-new-buffer "homework")))
    (switch-to-buffer homework-buffer)
    (cd (expand-file-name "~/homework"))
    (org-mode)
    (insert "__hw")
    (yas-expand-from-trigger-key)))

(defvar notes/directory (substitute-env-in-file-name "$HOME/notes"))
  (defvar notes/months-directory (concat notes/directory "/personal/todo"))

  (defun notes/month-label (&optional timestamp)
    (trim-trailing-newline
     (shell-command-to-string
      (concat "date "
              (and timestamp (format "-d @%s " timestamp))
              "+%Y-%m"))))

  (defun notes/list-month-notes ()
    (directory-files notes/months-directory nil (rx (= 4 num) "-" (= 2 num) ".org")))

  (defun notes/month-file (&optional month-label)
    (format "%s/%s.org"
            notes/months-directory
            (or month-label
                (notes/month-label))))

  (defun notes/month (&optional month-label)
    (interactive)
    (find-file (notes/month-file month-label)))
