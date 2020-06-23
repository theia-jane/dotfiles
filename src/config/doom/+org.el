;;; ~/Projects/dotfiles/src/config/doom/+org.el -*- lexical-binding: t; -*-

(use-package! org-crypt
  :after org
  :defer-incrementally t
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt"))
        org-crypt-key nil))

(use-package! doct
  :commands (doct))


(after! org
  (setq org-directory "~/notes"
        org-agenda-files `(,(f-join org-directory "projects"))
        org-todo-keywords '((sequence "TODO" "STARTED" "|" "DONE" "CANCELED"))
        org-refile-targets '((nil :maxlevel . 7)
                             (org-agenda-files :maxlevel . 1))
        org-refile-allow-creating-parent-nodes t
        org-src-ask-before-returning-to-edit-buffer nil
        org-outline-path-complete-in-steps nil    ; Refile in a single go
        org-refile-use-outline-path 'file         ; Show full paths for refiling
        org-highlight-latex-and-related nil ;; '(latex)
        org-M-RET-may-split-line '((default . nil)) ; don't split headings...
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil
        org-plantuml-jar-path (expand-file-name "~/java/plantuml/plantuml.jar")
        org-display-inline-images t
        org-pretty-entities nil
        org-startup-with-inline-images "inlineimages"
        org-hide-emphasis-markers nil
        org-export-with-toc nil
        org-export-with-section-numbers nil
        org-export-time-stamp-file nil
        org-export-async-init-file (expand-file-name "org-export-init.el" (dir!))
        org-src-window-setup 'current-window
        org-list-allow-alphabetical t)

  ;; A start, but I want to add a lot more rotations!
  ;; - block type
  ;; - option 'yes', 'no'
  ;; - results types (output, etc)
  ;; - list rotation, todo rotation (might make sense to create a DWIM rotation)
  (set-rotate-patterns! 'org-mode
    :symbols `(
               ;; TODO doesn't like compose, figure out why..
               ,(sort (cons "emacs-lisp"
                            (mapcar (-compose #'downcase #'car) org-src-lang-modes))
                      #'string<)
               ("yes" "no"))
    :patterns (list (cons
                     (rx line-start
                         "#+"
                         (group (+ word))
                         ":")
                     (sort (cl-delete-duplicates
                            (mapcar (lambda (keyword)
                                      (concat "#+" keyword))
                                    org-options-keywords)
                            :test #'equal)
                           #'string<)))))




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
               ;; ("Interesting" :keys "i"
               ;;  :icon ("eye" :set "faicon" :color "lcyan")
               ;;  :file +org-capture-todo-file
               ;;  :prepend t
               ;;  :headline "Interesting"
               ;;  :type entry
               ;;  :template ("* [ ] %{desc}%? :%{i-type}:"
               ;;             "%i %a")
               ;;  :children (("Webpage" :keys "w"
               ;;              :icon ("globe" :set "faicon" :color "green")
               ;;              :desc "%(org-cliplink-capture) "
               ;;              :i-type "read:web"
               ;;              )
               ;;             ("Article" :keys "a"
               ;;              :icon ("file-text" :set "octicon" :color "yellow")
               ;;              :desc ""
               ;;              :i-type "read:reaserch"
               ;;              )
               ;;             ("Information" :keys "i"
               ;;              :icon ("info-circle" :set "faicon" :color "blue")
               ;;              :desc ""
               ;;              :i-type "read:info"
               ;;              )
               ;;             ("Idea" :keys "I"
               ;;              :icon ("bubble_chart" :set "material" :color "silver")
               ;;              :desc ""
               ;;              :i-type "idea"
               ;;              )))
               ;; ("Tasks" :keys "k"
               ;;  :icon ("inbox" :set "octicon" :color "yellow")
               ;;  :file +org-capture-todo-file
               ;;  :prepend t
               ;;  :headline "Tasks"
               ;;  :type entry
               ;;  :template ("* TODO %? %^G%{extra}"
               ;;             "%i %a")
               ;;  :children (("General Task" :keys "k"
               ;;              :icon ("inbox" :set "octicon" :color "yellow")
               ;;              :extra ""
               ;;              )
               ;;             ("Task with deadline" :keys "d"
               ;;              :icon ("timer" :set "material" :color "orange" :v-adjust -0.1)
               ;;              :extra "\nDEADLINE: %^{Deadline:}t"
               ;;              )
               ;;             ("Scheduled Task" :keys "s"
               ;;              :icon ("calendar" :set "octicon" :color "orange")
               ;;              :extra "\nSCHEDULED: %^{Start time:}t"
               ;;              )
               ;;             ))
               ;; ("Project" :keys "p"
               ;;  :icon ("repo" :set "octicon" :color "silver")
               ;;    :prepend t
               ;;    :type entry
               ;;    :headline "Inbox"
               ;;    :template ("* %{time-or-todo} %?"
               ;;               "%i"
               ;;               "%a")
               ;;    :file ""
               ;;    :custom (:time-or-todo "")
               ;;    :children (("Project-local todo" :keys "t"
               ;;                :icon ("checklist" :set "octicon" :color "green")
               ;;                :time-or-todo "TODO"
               ;;                :file +org-capture-project-todo-file)
               ;;               ("Project-local note" :keys "n"
               ;;                :icon ("sticky-note" :set "faicon" :color "yellow")
               ;;                :time-or-todo "%U"
               ;;                :file +org-capture-project-notes-file)
               ;;               ("Project-local changelog" :keys "c"
               ;;                :icon ("list" :set "faicon" :color "blue")
               ;;                :time-or-todo "%U"
               ;;                :heading "Unreleased"
               ;;                :file +org-capture-project-changelog-file))
               ;;    )
               )))

(use-package! ox-extra
  :after org
  :defer-incrementally t
  :config
  (ox-extras-activate '(ignore-headlines)))

(set-file-template! 'org-mode
  :when '(lambda (file)
           (file-in-directory-p file "~/homework"))
  :trigger "__hw"
  :mode 'org-mode)

(setq +pretty-code-symbols (append
                            `(:title ,(propertize "" 'display '(raise 1))
                              :author ,(propertize "" 'display '(raise 0.1))
                              :setting ,(propertize "" 'display '(raise 0.1))
                              :latex ,(all-the-icons-fileicon "tex")
                              :property ,(all-the-icons-octicon "chevron-right"))
                            +pretty-code-symbols))

(set-pretty-symbols! 'org-mode
  :name "#+name:"
  :src_block "#+begin_src"
  :src_block_end "#+end_src"
  :src_block "#+begin_example"
  :src_block_end "#+end_example"
  :src_block "#+BEGIN_EXAMPLE"
  :src_block_end "#+END_EXAMPLE"
  :title "#+TITLE:"
  :setting "#+PROPERTY:"
  :author "#+AUTHOR:"
  :latex "#+LATEX_HEADER:"
  :latex "#+LATEX_HEADER_EXTRA:"
  :property  "#+STARTUP:"
  :property "#+RESULTS:")

(defun +org/goto-next-src-block ()
  (interactive)
  (org-next-block 1 nil "^[ \t]*#\\+begin_src")
  (+ui/smart-scroll-to-top))

(defun +org/eval-src-block-then-next ()
  (interactive)
  (condition-case _err
      (org-babel-execute-src-block)
    (t
     (+org/goto-next-src-block)
     (org-babel-execute-src-block)))
  (+org/goto-next-src-block))

(defun +tw/org-ctrl-c-ret ()
  (interactive)
  (org-ctrl-c-ret)
  (evil-insert-state))

(defvar +org-babel-eval-in-repl-candidates '("bash" "sh"))

(defun +org/babel-src-get-dir (header-args)
  (let ((dir (cdr (assq :dir header-args))))
    (if (and dir (not (file-remote-p dir)))
        dir
      default-directory)))

(defun +org/babel-eval-in-repl ()
  "TODO"
  (interactive)
  (destructuring-bind
      (lang contents header-args &rest _)
      (org-babel-get-src-block-info)
    (cond ((member lang org-babel-shell-names)
           (if (not (equal lang "eshell"))
               (+org-babel-src-evaluate-as-script
                lang
                (org-babel-expand-body:generic
                 contents header-args (org-babel-variable-assignments:shell header-args))
                header-args))))))

(defun +org-babel-src-evaluate-as-script (lang contents header-args)
  "TODO"
  (let* ((dir (+org/babel-src-get-dir header-args))
         (stdin (let ((stdin (cdr (assq :stdin header-args)))
                      (inhibit-message t))
                  (when stdin
                    (org-babel-sh-var-to-string (org-babel-ref-resolve stdin)))))
         (cmdline (cdr (assq :cmdline header-args)))
         (shebang (or
                   (cdr (assq :shebang header-args))
                   (format "#!/usr/bin/env %s" lang)))
         (script-file (org-babel-temp-file (concat lang "-script-")))
         (stdin-file (org-babel-temp-file "stdin-"))
         (padline (not (string= "no" (cdr (assq :padline header-args))))))
    (with-temp-file script-file
      (insert shebang "\n")
      (when padline (insert "\n"))
      (insert contents))
    (set-file-modes script-file #o755)
    (let ((command-string
           (cond
            ((or stdin cmdline)
             (with-temp-file stdin-file (insert (or stdin "")))
             (concat "cat " stdin-file " | " script-file (and cmdline (concat " " cmdline))))
            (t script-file))))
      (+eshell-run-command-visibly command-string dir))))

(defun +org-can-eval-in-repl? (element)
  "TODO"
  (let* ((type (org-element-type element))
         (lang (org-element-property :language element)))
    (and (eq type 'src-block)
         (member lang +org-babel-eval-in-repl-candidates))))

(defun +org/M-S-Return (arg)
  "Change what happens depending on where the point is."
  (interactive "P")
  (let* ((element (org-element-at-point)))
    (cond ((+org-can-eval-in-repl? element)
           (+org/babel-eval-in-repl))
          (t (org-insert-todo-heading arg)))))

(map! (:map org-mode-map
       "<S-return>" 'org-ctrl-c-ctrl-c
       "<M-S-return>" '+org/M-S-Return
       :ni "<C-S-return>" '+org/eval-src-block-then-next
       :n "M-i" #'org-edit-special
       "<normal-state> <M-return>" '+tw/org-ctrl-c-ret)
      (:map evil-org-mode-map
       :ni "<C-S-return>" '+org/eval-src-block-then-next
       ;; evil-org text objects
       :vo "ae" #'evil-org-an-object
       :vo "ie" #'evil-org-inner-object
       :vo "aE" #'evil-org-an-element
       :vo "iE" #'evil-org-inner-element
       :vo "ir" #'evil-org-inner-greater-element
       :vo "ar" #'evil-org-a-greater-element
       :vo "aR" #'evil-org-a-subtree
       :vo "iR" #'evil-org-inner-subtree
       :vn "zn" #'+evil:narrow-buffer

       ))

(defvar +font-lock-icons-default-prefix-format "\\(%s\\)" "TODO")

(defvar +font-lock-icons-default-icon-resolver #'identity
  "TODO")

(defun +prettify--compose (symbol)
  "Compose a sequence of characters into a symbol.
Regexp match data 0 specifies the characters to be composed."
  ;; Check that the chars should really be composed into a symbol.
  (let ((start (match-beginning 1))
        (end (match-end 1))
        (match (match-string 1)))
    (if (and (not (equal prettify-symbols--current-symbol-bounds (list start end)))
             (funcall prettify-symbols-compose-predicate start end match))
        (with-silent-modifications
          (when (= 1 (length symbol))
            (compose-region start end symbol))

          (add-text-properties
           start end
           `(prettify-symbols-start ,start
                                    prettify-symbols-end ,end
                                    display ,symbol)))
      ;; No composition for you.  Let's actually remove any
      ;; composition we may have added earlier and which is now
      ;; incorrect.
      (remove-list-of-text-properties start end
                                      '(display
                                        composition
                                        prettify-symbols-start
                                        prettify-symbols-end))))
  ;; Return nil because we're not adding any face property.
  nil)

(defadvice! +prettify-symbols--extended--post-command-hook ()
  ""
  :override 'prettify-symbols--post-command-hook
  (cl-labels ((get-prop-as-list
               (prop)
               (remove nil
                       (list (get-text-property (point) prop)
                             (when (and (eq prettify-symbols-unprettify-at-point 'right-edge)
                                        (not (bobp)))
                               (get-text-property (1- (point)) prop))))))
    ;; Re-apply prettification to the previous symbol.
    (when (and prettify-symbols--current-symbol-bounds
               (or (< (point) (car prettify-symbols--current-symbol-bounds))
                   (> (point) (cadr prettify-symbols--current-symbol-bounds))
                   (and (not (eq prettify-symbols-unprettify-at-point 'right-edge))
                        (= (point) (cadr prettify-symbols--current-symbol-bounds)))))
      (apply #'font-lock-flush prettify-symbols--current-symbol-bounds)
      (setq prettify-symbols--current-symbol-bounds nil))
    ;; Unprettify the current symbol.
    (when-let* ((c (or (get-prop-as-list 'display)
                       (get-prop-as-list 'composition)))
                (s (get-prop-as-list 'prettify-symbols-start))
                (e (get-prop-as-list 'prettify-symbols-end))
                (s (apply #'min s))
                (e (apply #'max e)))
      (with-silent-modifications
        (setq prettify-symbols--current-symbol-bounds (list s e))
        (remove-text-properties s e '(composition nil))
        (remove-text-properties s e '(display nil))
        ))))

(defun +font-lock-icons--parse-args (args)
  (let ((prefix-format +font-lock-icons-default-prefix-format)
        (icon-resolver +font-lock-icons-default-icon-resolver)
        (mode-font-lock-alist '((nil . nil)))
        mode)

    (while args
      (let ((arg (pop args))
            (next-arg (pop args)))

        (pcase arg
          (:prefix-format
           (setq prefix-format
                 (cond ((or (equal "" next-arg)
                            (null next-arg))
                        +font-lock-icons-default-prefix-format)
                       ((stringp next-arg) next-arg)
                       (t (error ":prefix-format must be a string or nil" next-arg)))))
          (:icon-resolver
           (if (functionp next-arg)
               (setq icon-resolver next-arg)
             (error ":icon-resolver must be a function")))
          (:mode
           (if (symbolp next-arg)
               (progn
                 (setq mode next-arg
                       icon-resolver +font-lock-icons-default-icon-resolver)
                 (unless (alist-get mode mode-font-lock-alist)
                   (push `(,mode . nil) mode-font-lock-alist)))
             (error ":mode must be a symbol for a mode")
             ))

          ;; Actual mapping
          ((pred stringp)
           (if (stringp next-arg)
               (let ((resolved-pattern (format prefix-format arg))
                     (resolved-icon `(funcall #',icon-resolver ,next-arg)))
                 (setq mode-font-lock-alist (cons
                                             (cons mode (cons
                                                         (list resolved-pattern
                                                               (list 1 `(progn (+prettify--compose (funcall #',icon-resolver ,next-arg)) nil) ))
                                                         (cdr (assq mode mode-font-lock-alist))))
                                             (assoc-delete-all mode mode-font-lock-alist))))
             (error "Icon must be a string")))
          (_ (error "Unsupported key or type: '%s'" arg)))))
    mode-font-lock-alist))

(defun +font-lock-icons! (&rest args)
  (let ((mode-lock-list (+font-lock-icons--parse-args args)))
    (dolist (mode-lock mode-lock-list)
      (when (cdr mode-lock)
        (font-lock-add-keywords (car mode-lock) (cdr mode-lock))))
    mode-lock-list))

(+font-lock-icons!
   :mode 'org-mode
   :prefix-format "^\\*+ \\(%s\\)"
   "TODO" "⚑"
   "STRT" "⚐"
   "STARTED" "⚐"
   "CANCELED" "✘"
   "DONE" "✔"


   :prefix-format (rx "#+" (or "begin" "BEGIN")
                      "_" (or "src" "SRC" "example" "EXAMPLE")
                      " " (group "%s"))
   :icon-resolver #'all-the-icons-fileicon
   "emacs-lisp" "emacs"
   "elisp" "emacs"
   "bash" "terminal"
   "sh" "terminal"

   :icon-resolver #'all-the-icons-alltheicon
   "python" "python"
   )

;;; Babel
(defadvice! +org-babel--resolve-tangle-path-to-dir-a (fn &optional light datum)
  "Add :tangle-relative property to org babel header args.

This new property will make the :tangle files relative to
the :dir or to the value of :tangle-relative.

If :tangle-relative is
- equal to 'dir, then it uses :dir
- a string it uses the value passed
"
  :around #'org-babel-get-src-block-info
  (let ((info (funcall fn light datum)))
    (unless light
      (let* ((prop-alist (nth 2 info))
             (dir (alist-get :dir prop-alist))
             (tangle (alist-get :tangle prop-alist))
             (tangle-relative (alist-get :tangle-relative prop-alist)))
        (when (and dir
                   (not (equal tangle "yes"))
                   (not (equal tangle "no"))
                   tangle-relative)
          (setf (alist-get :tangle prop-alist)
                (f-join (cond
                         ((stringp tangle-relative) tangle-relative)
                         ((eq tangle-relative 'dir) dir)
                         (t ""))
                        tangle)))))
  info))


(defadvice! +org-babel--buffer-arg (fn result &optional result-params info hash lang)
  "Popup results of an org src block if :buffer t is given as a header argument "
  :around  'org-babel-insert-result ;; probably a better type of advice
  (funcall fn result result-params info hash lang)
  ;; (message "%s" (pp-to-string info))
  (when-let ((buffer-val (alist-get :buffer (nth 2 info))))
    (let* ((result-buffer-name (concat "*org:results:" (buffer-name) "["  hash "]*" ))
           (result-buffer (get-buffer-create result-buffer-name)))
      (with-current-buffer result-buffer
        (erase-buffer)
        (insert result)
        (+popup-buffer (current-buffer))))))

