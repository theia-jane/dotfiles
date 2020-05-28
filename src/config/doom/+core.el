;;; ~/Projects/dotfiles/src/config/doom/+core.el -*- lexical-binding: t; -*-

(defun +buffer--display-in-vertical-split (buffer alist)
  "Uses an existing window to the right or left to display the BUFFER.
Otherwise it creates a window on the right and displays the BUFFER."
  (if-let ((side-window (or (window-in-direction 'right)
                         (window-in-direction 'left))))
        (with-selected-window side-window
          (display-buffer-same-window buffer nil))
    (display-buffer-in-direction buffer '((direction . right)))))

(defun +buffer--display-in-vertical-split-maybe (buffer alist)
  "Wraps around `+buffer--display-in-vertical-split', but only
displays the buffer if it isn't already in a window in this frame."
  (or (get-buffer-window buffer nil)
    (+buffer--display-in-vertical-split buffer alist)))

(defun +buffer-open-in-vertical-split-maybe (buffer-or-name &optional force)
  "Open the BUFFER-OR-NAME in a vertical split, but only if the buffer isn't
already displayed.

If FORCE is set, it overrides any other actions down in `display-buffer' by setting
`display-buffer-overriding-action'. For those times you really, really want things to
do as you say."
  (let ((action '(+buffer--display-in-vertical-split-maybe . nil))
        (display-buffer-overriding-action display-buffer-overriding-action))
    (when force
      (setq display-buffer-overriding-action action))
    (display-buffer buffer-or-name action)))


(defun +open-messages ()
  "Open messages buffer in vertical split, if it isn't already open."
  (interactive)
  (+buffer-open-in-vertical-split-maybe (messages-buffer) t)
  (with-current-buffer (messages-buffer)
    (set-window-point (get-buffer-window (current-buffer)) (point-max))))


(defun +ensure-buffer (name &optional setup-fn)
  (let ((buffer (get-buffer name)))
    (unless buffer
      (setq buffer (get-buffer-create name))
      (with-current-buffer buffer
          (funcall setup-fn)))
    buffer))
(defun +find-file (filename &optional wildcards)
  "Like `find-file', but with path joining.

If FILENAME is a list of file paths, then join them together.
For example:

    (+find-file '(\"/tmp/\" \"path\" \"file\"))

Will call `find-file' with the file \"/tmp/path/file\".

For WILDCARDS see `find-file'.
"
  (interactive
   (find-file-read-args "Find file: "
                        (confirm-nonexistent-file-or-buffer)))
  (when (listp filename)
    (setq filename (apply #'f-join filename)))
  (find-file filename wildcards))
(defvar doom-map-extra-mapping-fns nil)

(defun doom-map-extra-mapping-key-p (key)
  (plist-get doom-map-extra-mapping-fns key))

(defadvice! +doom--map-process (rest)
  :override 'doom--map-process
  (let ((doom--map-fn doom--map-fn)
        doom--map-state
        doom--map-forms
        desc)
    (while rest
      (let ((key (pop rest)))
        (cond ((listp key)
               (doom--map-nested nil key))

              ((keywordp key)
               (pcase key
                 ((pred doom-map-extra-mapping-key-p)
                  (doom--map-commit)
                  (setq doom--map-fn (plist-get doom-map-extra-mapping-fns key)))
                 (:leader
                  (doom--map-commit)
                  (setq doom--map-fn 'doom--define-leader-key))
                 (:localleader
                  (doom--map-commit)
                  (setq doom--map-fn 'define-localleader-key!))
                 (:after
                  (doom--map-nested (list 'after! (pop rest)) rest)
                  (setq rest nil))
                 (:desc
                  (setq desc (pop rest)))
                 (:map
                  (doom--map-set :keymaps `(quote ,(doom-enlist (pop rest)))))
                 (:mode
                  (push (cl-loop for m in (doom-enlist (pop rest))
                                 collect (intern (concat (symbol-name m) "-map")))
                        rest)
                  (push :map rest))
                 ((or :when :unless)
                  (doom--map-nested (list (intern (doom-keyword-name key)) (pop rest)) rest)
                  (setq rest nil))
                 (:prefix-map
                  (cl-destructuring-bind (prefix . desc)
                      (doom-enlist (pop rest))
                    (let ((keymap (intern (format "doom-leader-%s-map" desc))))
                      (setq rest
                            (append (list :desc desc prefix keymap
                                          :prefix prefix)
                                    rest))
                      (push `(defvar ,keymap (make-sparse-keymap))
                            doom--map-forms))))
                 (:prefix
                  (cl-destructuring-bind (prefix . desc)
                      (doom-enlist (pop rest))
                    (doom--map-set (if doom--map-fn :infix :prefix)
                                   prefix)
                    (when (stringp desc)
                      (setq rest (append (list :desc desc "" nil) rest)))))
                 (:textobj
                  (let* ((key (pop rest))
                         (inner (pop rest))
                         (outer (pop rest)))
                    (push `(map! (:map evil-inner-text-objects-map ,key ,inner)
                                 (:map evil-outer-text-objects-map ,key ,outer))
                          doom--map-forms)))
                 (_
                  (condition-case _
                      (doom--map-def (pop rest) (pop rest)
                                     (doom--map-keyword-to-states key)
                                     desc)
                    (error
                     (error "Not a valid `map!' property: %s" key)))
                  (setq desc nil))))

              ((doom--map-def key (pop rest) nil desc)
               (setq desc nil)))))

    (doom--map-commit)
    (macroexp-progn (nreverse (delq nil doom--map-forms)))))

(defmacro define-leader! (name key &rest args)
  (let* ((name (symbol-name name))
         (key-symbol (intern (concat name "-leader-key")))
         (map-symbol (intern (concat name "-leader-map")))
         (command-symbol (intern (concat name "/leader")))
         (keyword-symbol (intern (format ":%s-leader" name)))
         (define-fn-symbol (intern (format "define-%s-leader-key!" name))))
    `(progn
       (defvar ,key-symbol ,key)
       (defvar ,map-symbol (make-sparse-keymap)
         ,(format "An overriding keymap for my <%s-leader> keys." name))

       (define-prefix-command ',command-symbol ',map-symbol)
       (define-key ,map-symbol [override-state] 'all)

       (add-hook! 'doom-after-init-modules-hook
         (defun ,(intern (concat name "-init-leader-keys-h")) ()
           ,(format "Bind `%s-leader-key'." name)
           (let ((map general-override-mode-map))
             (evil-define-key* '(normal visual motion emacs insert) map (kbd ,key-symbol) ',command-symbol)
             (general-override-mode +1))))

       (defmacro ,define-fn-symbol (&rest args)
         ,(format "Define <%s-leader> key.

Uses `general-define-key' under the hood, but does not support :major-modes,
:states, :prefix or :non-normal-prefix. Use `map!' for a more convenient
interface.

See `%s-leader-key' to change the work-leader prefix." name name)
         `(general-define-key
           :states '(normal visual motion emacs insert)
           :major-modes t
           :prefix ,,key-symbol
           ,@args)

         )


       (unless (plist-get doom-map-extra-mapping-fns ,keyword-symbol)
         (setq doom-map-extra-mapping-fns (append doom-map-extra-mapping-fns
                                                  '(,keyword-symbol
                                                        ,define-fn-symbol))))
       (use-package! which-key
         :config
         (which-key-add-key-based-replacements ,key-symbol ,(format "<%s-leader>" name)))
       )))
