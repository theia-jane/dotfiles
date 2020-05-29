;;; ~/Projects/dotfiles/src/config/doom/+magit.el -*- lexical-binding: t; -*-

(after! magit
  (require 'rextract)
  (defconst +magit--status-type-parse-defs
    '(("1" :status :submodule :file-modes (:head 1 :index 1 :worktree 1) :object-names (:head 1 :index 1) :path)
      ("2" :status :submodule :file-modes (:head 1 :index 1 :worktree 1) :object-names (:head 1 :index 1) :score :path :original-path)
      ("u" :status :submodule :file-modes (:stage-1 1 :stage-2 1 :stage-3 1 :worktree 1) :object-names (:stage-1 1 :stage-2 1 :stage-3 1 :index 1) :path)
      ("?" :path)
      ("!" :path))
    "TODO")

  (defun +magit-parse-status-line (status-line)
    "TODO"
    (let* ((status-type (substring status-line 0 1))
           (status-tokens (cons :type (cdr (assoc status-type +magit--status-type-parse-defs)))))

      (unless status-tokens
        (error "unknown git status type: %s" status-type))

      (cl-loop while status-tokens
               append
               (let ((status-token (pop status-tokens)))
                 (list
                  status-token
                  (pcase status-token
                    ((or :type :status :submodule)
                     (rextract-field status-line))
                    ((or :file-modes :object-names)
                     (rextract-fields status-line (pop status-tokens)))

                    (:score
                     (rextract-groups status-line (rx (group alpha) (group (+ num)) space)))

                    ((or :path :original-path)
                     (rextract-field status-line (rx (or "\0" eos))))))))))


  (defun +magit-parse-status-lines (status-lines)
    "TODO"
    (mapcar #'+magit-parse-status-line status-lines))

  (defun +magit-get-status-items (path)
    "TODO"
    (let ((default-directory path))
      (+magit-parse-status-lines
       (magit-git-items "status" "-z" "--porcelain=2"))))

  ;; Add some caching.. no need to parse them if we don't need to
  (add-transient-hook! '+magit-get-status-items
    (condition-case _
        (memoize-restore '+magit-get-status-items)
      (t nil))
    (memoize #'+magit-get-status-items 2))

  (defun +magit-all-commitable-files (&optional ignore-untracked)
    "TODO"
    (cl-loop for status in (+magit-get-status-items default-directory)
             append (unless (and ignore-untracked
                                 (equal (plist-get status :type) "?"))
                      (list (plist-get status :path)))))

  (defun +counsel-magit-commitable-files ()
    "TODO"
    (ivy-read (projectile-prepend-project-name "Review: ")
              (+magit-all-commitable-files)
              :require-match t))

  (defun +magit-review-file (file)
    "TODO"
    (interactive (list (+counsel-magit-commitable-files)))
    (let ((file-path (expand-file-name file (magit-toplevel))))
      (when (ediff-in-control-buffer-p)
        (ediff-quit nil))
      (vc-version-ediff (list file-path) nil nil)))

  ;; TODO: Get a keybinding for: next/previous uncommitted file
  ;; - It should take into account the current file
  ;; - If the current file isn't in the list, insert it into the list, sort and
  ;;   move to the next / prev in the list

  (defun +magit-review-all ()
    "TODO -- create a workflow for reviewing all the files

Maybe a UI driven by org?
* DONE <file/one>
Looks good, notes on things...
* STARTED <file/two>
* TODO <file/three>

MVP would be a list of files, a pointer and methods for switching files being diffed
"
    (interactive)
    (vc-version-ediff
     (cl-loop for file in (+magit-all-commitable-files t)
              collect (expand-file-name file (magit-toplevel)))
     nil nil))


  (defun +ediff-next-difference-or-file ()
    "TODO: Go to next difference. If that fails, go to next file"
    )
  )

(defadvice! +y-or-n-p--say-yes (fn &rest args)
  :around '(ediff-quit)
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
    (apply fn args)))


(defun +magit-get-previous-file (file)
  (let ((files (+magit-all-commitable-files)))
    (unless (cl-find file files :test #'equal)
      (setq files (sort (cons file files) #'string-lessp)))

    (let ((file-position (cl-position file files :test #'equal)))
      (when (zerop file-position)
        (user-error "No previous file"))

      (nth (1- file-position) files))))

(defun +magit-get-next-file (file)
  (let ((files (+magit-all-commitable-files)))
    (unless (cl-find file files :test #'equal)
      (setq files (sort (cons file files) #'string-lessp)))

    (let ((file-position (cl-position file files :test #'equal))
          (max-file-position (1- (length files))))
      (when (equal file-position max-file-position)
        (user-error "No next file"))
      (nth (1+ file-position) files))))

;; (let ((default-directory (substitute-env-vars "$ts_app")))
;;   ;; (+magit-all-commitable-files)
;;   ;; (+ediff-next-file "w")
;;   (+magit-review-file "src/Feedback/Feedback.php"))

(defun +magit-review-this-file ()
  (interactive)
  (+magit-review-file (magit-file-relative-name)))

(defun +ediff-next-file ()
  "TODO"
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (let ((ediff-buffer (or ediff-buffer-C
                          ediff-buffer-B)))
    (unless ediff-buffer
      (user-error "Missing ediff buffer"))

    (+magit-review-file
     (+magit-get-next-file
      (with-current-buffer ediff-buffer
        (magit-file-relative-name))))))


(defun +ediff-previous-file ()
  "TODO"
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (let ((ediff-buffer (or ediff-buffer-C
                          ediff-buffer-B)))
    (unless ediff-buffer
      (user-error "Missing ediff buffer"))

    (+magit-review-file
     (+magit-get-previous-file
      (with-current-buffer ediff-buffer
        (magit-file-relative-name))))))

(defadvice! +evil-ediff--keybinds (&rest _)
  :after 'evil-collection-ediff-startup-hook
  (define-key ediff-mode-map "\C-j" #'+ediff-next-file)
  (define-key ediff-mode-map "\C-k" #'+ediff-previous-file))
