;;; ~/Projects/dotfiles/src/config/doom/+org.el -*- lexical-binding: t; -*-

(use-package! org-crypt
  :after org
  :defer-incrementally t
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt"))
        org-crypt-key nil))

(after! org
  (setq org-directory "~/notes/"
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
        org-startup-folded 'overview
        org-hide-emphasis-markers nil
        org-export-with-toc nil
        org-export-with-section-numbers nil
        org-export-time-stamp-file nil
        org-export-async-init-file (expand-file-name "org-export-init.el" (dir!))
        org-src-window-setup 'current-window
        org-list-allow-alphabetical t)

  ;; http://bnbeckwith.com/blog/org-mode-tikz-previews-on-windows.html
  (add-to-list 'org-latex-packages-alist
               '("" "tikz" t))
  (after! preview
    (add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))
  ;; Note if you have issues w/ rendering might need to check your imagemagick policies
  ;;      /etc/ImageMagick-{6,7}/policy.xml
  ;;      It's possible that one of the policies is set to 'none' for something.
  ;; (setq org-preview-latex-default-process 'imagemagick)

  (setq org-preview-latex-default-process 'pdf2svg)
  (add-to-list
   'org-preview-latex-process-alist
   '(pdf2svg :programs
               ("latex" "pdf2svg" "rsvg-convert")
               :description "pdf > svg" :message "you need to install the programs: latex and pdf2svg." :image-input-type "pdf" :image-output-type "svg" :image-size-adjust
               (1 . 1)
               :latex-compiler
               ("pdflatex -interaction nonstopmode -output-directory %o -shell-escape %f")
               :image-converter
               ("pdf2svg %f %O-pre"
                "rsvg-convert -d %D -p %D %O-pre -f svg -o %O"
                "rm %O-pre"))
   )
  (defadvice! +org-svg-preview (fn &rest args)
    :around 'org-create-formula-image
    (let ((org-format-latex-header "\\documentclass{standalone}
\\usepackage[usenames]{color}
[PACKAGES]
[DEFAULT-PACKAGES]")
          (org-preview-latex-default-process 'pdf2svg))
      (apply fn args)))




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
                           #'string<))))

  ;; Make my org directory be a project root
  (defun +org-notes-root (dir)
    (and dir
         (file-in-directory-p dir org-directory)
         org-directory))
  (after! projectile
    (add-to-list 'projectile-project-root-files-functions '+org-notes-root))

  (load! "+org-babel"))



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
       :ni "M-o" #'org-cleave
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
       :vn "zn" #'+evil:narrow-buffer)
      (:localleader
       :map org-mode-map
       "n" nil ;; clear this out first (otherwise we'll get a "not a prefix key warning")
       (:prefix ("n" . "Narrow")
        :desc "Next heading" "]" #'+org-forward-to-narrowed-heading
        :desc "Next heading" "[" #'+org-backward-to-narrowed-heading
        :desc "Subtree" "s" #'org-narrow-to-subtree
        :desc "Block" "b" #'org-narrow-to-block
        :desc "Element" "e" #'org-narrow-element)
       :desc "Set Name" "N" #'org-set-name
       (:prefix ("L" . "Latex")
        :desc "Preview" "p" (cmd! (org--latex-preview-region (point-min) (point-max))))))



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

(defun org-cleave ()
  "TODO"
  (interactive)
  (let* ((el (org-element-at-point))
         (el-type (car el))
         (post-affiliated (plist-get (nth 1 el) :post-affiliated)))
    (when (eq el-type 'src-block)
      (let ((src-begin-line (save-excursion
                              (goto-char post-affiliated)
                              (buffer-substring (point) (line-end-position)))))
        (end-of-line)
        (insert (format "\n#+END_SRC\n\n%s\n" src-begin-line))))))

(defun org-src-content-bounds (el)
  (list
   (save-mark-and-excursion
     (goto-char (org-element-property :post-affiliated el))
     (line-end-position))
   (save-mark-and-excursion
     (goto-char (1- (org-element-property :end el)))
     (forward-line (- (org-element-property :post-blank el)))
     (line-beginning-position))))

(defun org-block-header (el)
  (save-mark-and-excursion
    (goto-char (org-element-property :post-affiliated el))
    (buffer-substring (point) (line-end-position))))

(defun org-ensure-noweb ()
  (let* ((info (org-babel-get-src-block-info))
         (noweb (alist-get :noweb (nth 2 info))))
    (when (or (not noweb)
              (equal noweb "no"))
      (org-babel-insert-header-arg "noweb" "yes"))))

(defun org-extract (beg end name)
  "TODO"
  (interactive
   (list (region-beginning)
         (region-end)
         (read-string "Name: " (org-get-name))))

  (when (and beg end)
    (let ((el (org-element-at-point)))
      (when (and (eq (car el) 'src-block))
        (let* ((src-header (org-block-header el))
               (content-bounds (org-src-content-bounds el))
               (has-name (and name (not (equal name ""))))
               (region-contents (buffer-substring beg end)))
          (when (and (<= (car content-bounds) beg (cadr content-bounds))
                     (<= (car content-bounds) end (cadr content-bounds)))
            (setq deactivate-mark t)
            (evil-exit-visual-state)
            (delete-region beg end)
            (when has-name
              (save-excursion
                (end-of-line)
                (org-ensure-noweb)
                (insert "<<" name ">>")))

            (goto-char (cadr (org-src-content-bounds (org-element-at-point))))
            (insert
             (concat
              (when (eq (forward-line) 1)
                "\n")
              "\n"
              (when has-name
                (concat "#+NAME: " name "\n"))
              src-header
              "\n"
              region-contents)
             )
            (save-excursion
              (insert "\n\n#+END_SRC\n"))

            ))))))

(defun test/x ()
  (goto-char 3332))

(defun org-rename ()
  "TODO"
  nil)


(defun org-get-name ()
  (let ((el (org-outer-element)))
    (or (org-element-property :name el)
        (and (equal (org-element-property :key el) "NAME")
             (org-element-property :value el)))))

(defun org-set-name (name)
  (interactive
   (list (read-string "Name: " (org-get-name))))
  (let* ((el (org-outer-element))
         (el-type (car el))
         (existing-name (org-get-name)))
    (when (and el-type
               (or (not (memq el-type '(headline keyword)))
                   (and (eq el-type 'keyword)
                        (equal (org-element-property :key el) "NAME")))

               (save-excursion
                 (goto-char (org-element-property :begin el))

                 (unless existing-name
                   (beginning-of-line)
                   (save-excursion
                     (insert "#+NAME:\n")))

                 (if (or (null name) (equal name ""))
                     (delete-region (point) (1+ (line-end-position)))
                   (save-match-data
                     (re-search-forward (rx "#+NAME:"))
                     (delete-region (match-end 0) (line-end-position)))
                   (end-of-line)
                   (insert (concat " " name))))))))

(defun org-parent-property (property context)
  (when-let ((parent (org-element-property :parent context)))
    (org-element-property property parent)))

(defun org-inherited-property (property context)
  (or (org-element-property property context)
      (org-parent-property property context)))

(defun org-top-level-property (property context)
  (or (org-parent-property property context)
      (org-element-property property context)))

(defun org-outer-element (&optional context)
  (let* ((el (or context (org-element-at-point)))
         (parent (org-element-property :parent el)))
    (or parent el)))

(defun +org-forward-to-narrowed-heading (arg)
  (interactive "p")
  (when (buffer-narrowed-p)
    (goto-char (point-min))
    (org-forward-heading-same-level arg)
    (widen))
  (org-forward-heading-same-level p)
  (org-narrow-to-subtree))

(map!
 :nivo "M-i" doom-localleader-key)

(defun doom-send-localleader ()
  (interactive)
  (setq prefix-arg current-prefix-arg)
  (setq unread-command-events (append (listify-key-sequence (kbd "SPC")) '(?m))))


(defun +org-backward-to-narrowed-heading (arg)
  (interactive "p")
  (+org-forward-to-narrowed-heading (- arg)))