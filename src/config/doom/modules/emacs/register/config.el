;;; emacs/register/config.el -*- lexical-binding: t; -*-

(after! evil
  (defvar +register-get-register-as-string-p nil
    "Controls the behavior of `+register-value-to-string'. If nil then
don't convert the register contents to a string.")

  (defadvice! +register--evil-execute-macro--execute-elisp-too (args)
    "Allows execution of elisp stored in plain text."
    :filter-args 'evil-execute-macro
    (let ((count (nth 0 args))
          (macro (nth 1 args)))
      (when (+register--elisp-string-p macro)
        (setq macro `(lambda () ,(car (read-from-string (substring-no-properties macro))))))
      (list count macro)))

  (defadvice! +register--evil-get-register-a (fn &rest args)
    "General extension fo the capabilities of `evil-get-register'.

  - Enables broader use of special ex-registers (e.g. C-w, C-W, C-o, C-f)
  - Converts register contents to string depending on if
    `+register-get-register-as-string-p' is non-nil."
    :around #'evil-get-register
    (let ((evil-ex-current-buffer evil-ex-current-buffer))
      (when (memq major-mode '(minibuffer-inactive-mode))
        (with-selected-window (minibuffer-selected-window)
          (setq evil-ex-current-buffer (current-buffer))))
      (let ((output (apply fn args)))
        (if +register-get-register-as-string-p
            (+register-value-to-string output)
          output))))

  (defadvice! +register--evil-paste-special-a (fn &rest args)
    "Sets `+register-get-register-as-string-p' to t, so
pasting sexps from a register will succeed."
    :around '(evil-paste-after
              evil-paste-before
              evil-paste-from-register
              evil-visual-paste
              evil-goggles--paste-vert-block-p)
    (let ((+register-get-register-as-string-p t))
      (apply fn args))))


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


;;; helper methods
(defun +register-value-to-string (register-value)
  "TODO"
  (cond ((stringp register-value) register-value)
        ((vectorp register-value) (+register-key-sequence-to-string register-value))
        ((consp register-value)
         (let ((print-quoted t)
               (print-length nil)
               (print-level nil))
           (prin1-to-string register-value)))
        (t (error "Not able to get the register as a string"))))


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



(map! (:after evil
       :map evil-normal-state-map "g E" #'evil:register-edit)
      (:map +register-edit-mode-map
       :desc "Save to register and close" "C-c C-c" #'+register-edit-mode--save
       :desc "Abort register edit" "C-c C-k" #'kill-current-buffer))
