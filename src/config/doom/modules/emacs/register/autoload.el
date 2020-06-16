;;; emacs/register/autoload.el -*- lexical-binding: t; -*-

(defvar +register-get-register-as-string-p nil
  "Controls the behavior of `+register-value-to-string'. If nil then
don't convert the register contents to a string.")

(defadvice! +register--evil-execute-macro--execute-elisp-too (args)
  "Allows execution of elisp stored in plain text."
  :filter-args 'evil-execute-macro
  (let ((count (nth 0 args))
        (macro (nth 1 args)))
    (when-let ((elisp-form (+register--get-callable-from-value macro)))
      (setq macro elisp-form))
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
(defvar-local +register--register-type nil)
(put '+register--register-type 'permanent-local t)

(defvar-local +register--previous-contents nil)
(put '+register--previous-contents 'permanent-local t)

(defvar-local +register--register nil)
(put '+register--register 'permanent-local t)

(define-minor-mode +register-edit-mode
  "Minor mode for editting a register."
  :init-val nil
  :lighter ""
  :keymap (make-sparse-keymap)
  (setq header-line-format "Edit, then exit with `C-c C-c' or abort with `C-c C-k'"))

(defun +register-edit-mode-save ()
  "Save the current register-edit buffer to it's register and close the buffer. "
  (interactive)
  (unless +register--register
    (user-error "Not in a register edit buffer or register unknown"))

  (let ((register-contents (substring-no-properties (buffer-string))))
    (set-register +register--register
                  (pcase +register--register-type
                    ('elisp (if (and (+register--get-callable-from-value register-contents)
                                     (consp +register--previous-contents))
                                (car (read-from-string register-contents))
                              register-contents))
                    ('key-sequence (kbd register-contents))
                    (_ register-contents))))
  (kill-current-buffer))

(map! :map +register-edit-mode-map
      :desc "Save to register and close" "C-c C-c" #'+register-edit-mode-save
      :desc "Abort register edit" "C-c C-k" #'kill-current-buffer)

;;; helper methods
(defun +register-value-to-string (register-value)
  ""
  (cond ((stringp register-value) register-value)
        ((vectorp register-value) (+register-key-sequence-to-string register-value))
        ((or (symbolp register-value)
             (consp register-value)) (prin1-to-string register-value))
        (t (error "Not able to get the register as a string"))))






(defun +register-key-sequence-to-string (key-sequence)
  "TODO"
  (if (vectorp key-sequence)
      (mapconcat #'(lambda (key)
                     (key-description `[,key]))
                 key-sequence
                 " ")
    (user-error "Not a valid key sequence")))


(defun +register--get-callable-from-value (value)
  "Get a callable version of the VALUE.

If VALUE is a string, it will be read first. If the resulting form or it's symbol
is callable, then it will be wrapped in a lambda, so it can be evaluated.

If the form is something that isn't callable (e.g a string, vector, etc)
then nil is returned.
"

  (let* ((form (if (stringp value)
                   (car (read-from-string value))
                 value)))
    (cond
     ((and (symbolp form)
           (fboundp form)) (lambda () (eval (list form))))
     ((or
       (functionp form)
       (and (consp form)
            (memq (car form) '(lambda function)))) form)
     ((and (consp form)
           (symbolp (car form))
           (fboundp (car form))) `(lambda () ,form))
     (t nil))))


;;;###autoload
(defun +register-edit (register)
  "Edit a REGISTER's value. This will pop to a buffer to edit said value."
  (let* ((register-contents (get-register register))
         (register-type (cond
                         ((or (symbolp register-contents)
                              (consp register-contents)
                              (+register--get-callable-from-value register-contents)) 'elisp)
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

