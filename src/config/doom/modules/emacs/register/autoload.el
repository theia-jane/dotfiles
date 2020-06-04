;;; emacs/register/autoload.el -*- lexical-binding: t; -*-

(defvar +register-get-register-as-string-p nil
  "Controls the behavior of `+register-value-to-string'. If nil then
don't convert the register contents to a string.")

(defadvice! +register--evil-execute-macro--execute-elisp-too (args)
  "Allows execution of elisp stored in plain text."
  :filter-args 'evil-execute-macro
  (let ((count (nth 0 args))
        (macro (nth 1 args)))
    (when (+register--elisp-string-p macro)
      (let ((elisp-form (car (read-from-string (substring-no-properties macro)))))
        (setq macro (if (and (listp elisp-form)
                             (eq 'lambda (car elisp-form)))
                        elisp-form
                      `(lambda () ,elisp-form)))))
    (list count macro)))

(defadvice! +register--evil-get-register-a (register-value)
  "If `+register-get-register-as-string-p' is non-nil
then try to convert the REGISTER-VALUE to a string."
  :filter-return 'evil-get-register
  (+register-value-to-string register-value))

(defadvice! +register--evil-paste-special-a (fn &rest args)
  "Sets `+register-get-register-as-string-p' to t, so
pasting sexps from a register will succeed."
  :around '(evil-paste-after
            evil-paste-before
            evil-paste-from-register
            evil-visual-paste
            evil-goggles--paste-vert-block-p)
  (let ((+register-get-register-as-string-p t))
    (apply fn args)))

;;;###autoload (autoload 'evil:register-edit "modules/emacs/register/autoload" nil t)
(evil-define-command evil:register-edit (register)
  "Edit a REGISTER. This passes through to `+register-edit'."
  :suppress-operator t
  (interactive
   (list (or evil-this-register (evil-read-key))))
  (+register-edit register))


;;; Register Edit Mode
(define-minor-mode +register-edit-mode
  "TODO"
  :init-val nil
  :lighter ""
  :keymap (make-sparse-keymap)
  (setq header-line-format "Edit, then exit with `C-c C-c' or abort with `C-c C-k'"))

(defvar-local +register--register-type nil)
(defvar-local +register--previous-contents nil)
(defvar-local +register--register nil)

(defun +register-edit-mode--save ()
  "TODO"
  (interactive)
  (unless +register--register
    (user-error "Not in a register edit buffer"))

  (let ((register-contents (substring-no-properties (buffer-string))))
    (set-register +register--register
                  (pcase +register--register-type
                    ('elisp (if (and (+register--elisp-string-p register-contents)
                                     (consp +register--previous-contents))
                                (car (read-from-string register-contents))
                              register-contents))
                    ('key-sequence (kbd register-contents))
                    (_ register-contents))))
  (kill-current-buffer))

(map! :map +register-edit-mode-map
      :desc "Save to register and close" "C-c C-c" #'+register-edit-mode--save
      :desc "Abort register edit" "C-c C-k" #'kill-current-buffer)

;;; helper methods
(defun +register-value-to-string (register-value)
  "TODO"
  (cond ((stringp register-value) register-value)
        ((vectorp register-value) (key-sequence-to-string register-value))
        ((consp register-value)
         (let ((print-quoted t)
               (print-length nil)
               (print-level nil))
           (prin1-to-string register-value)))
        (t (error "Not able to get te hregister as a string"))))


(defun +register-key-sequence-to-string (key-sequence)
  "TODO"
  (if (vectorp key-sequence)
      (mapconcat #'(lambda (key)
                     ()
                     (key-description `[,key]))
                 key-sequence
                 " ")
    (user-error "Not a valid key sequence")))

(defun +register--elisp-string-p (text)
  "TODO"
  (and (stringp text)
       (string-match "^\(\\([^\t\r\n\s\(\)]+\\)\s" text)
       (symbolp (intern (match-string 1 text)))))

;;;###autoload
(defun +register-edit (register)
  "Edit a REGISTER's value. This will pop to a buffer to edit said value."
  (let* ((register-contents (get-register register))
         (register-type (cond
                         ((or (consp register-contents)
                              (+register--elisp-string-p register-contents)) 'elisp)
                         ((vectorp register-contents) 'key-sequence)
                         (t 'plain)))
         (buffer (get-buffer-create (concat "*+register-edit:" (char-to-string register) "*"))))
    (with-current-buffer buffer
      (erase-buffer)
      (when register-contents
        (insert (+register-value-to-string register-contents))
        (set-buffer-modified-p nil)
        (when (eq register-type 'elisp)
          (emacs-lisp-mode)
          (pp-buffer)))

      (setq buffer-file-name nil
            +register--register-type register-type
            +register--register register
            +register--previous-contents register-contents)

      (+register-edit-mode)
      (pop-to-buffer buffer))))

