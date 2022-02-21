;;; -*- lexical-binding: t; -*-
(provide 'eval-context)
(require 'personal-lib)

(defvar eval-context-default-mode 'emacs-lisp-mode
  "When creating a new eval-context buffer what mode should we use?")

(defvar-local eval-context-buffer nil
  "Local variable indicating what buffer to evaluate the elisp in.")
(put 'eval-context-buffer 'permanent-local t)

(defvar-local eval-context-edebug nil
  "Local variable indicating if we are evaluating elisp in the edebug context.")
(put 'eval-context-edebug 'permanent-local t)

(defun eval-context-make-name (name &optional ielm-p)
  (format "*eval-context%s[%s]*"
          (if ielm-p ":ielm" "") 
          name))

(defun eval-context-set-buffer (&optional buffer)
  "Set the BUFFER as the current eval-context."
  (interactive (list (eval-context-buffer-select)))
  (setq eval-context-buffer (and buffer (get-buffer buffer)))
  (eval-context-update-mode-maybe))

(defun eval-context-set-edebug (&optional arg)
  "Set the edebug eval-context flag with ARG.

Defaults to `1'. non-nil indicates an we are evaluating in that state context."
  (interactive (list 1))
  (setq eval-context-edebug arg)
  (eval-context-update-mode-maybe))


(defun open-eval-context-buffer-buffer (&optional context-buffer mode)
  "Open a buffer to evaluate elisp in the CONTEXT-BUFFER.

Optionally, specifing MODE of this new buffer.

CONTEXT-BUFFER defaults to the current buffer.

MODE defaults to `eval-context-default-mode'."
  (interactive (list (current-buffer)))
  (let* ((name (eval-context-make-name (buffer-name context-buffer)))
         (buffer (get-buffer name)))
    (unless buffer
      (setq buffer (get-buffer-create name))
      (with-current-buffer buffer
        (funcall (or mode eval-context-default-mode))
        (eval-context-set-buffer context-buffer)))
  (pop-to-buffer buffer)))

(defun open-eval-context-edebug-buffer (&optional mode)
  "Open a buffer to evaluate elisp in the debug context.

Optionally, specifing MODE of this new buffer.

MODE defaults to `eval-context-default-mode'."
  (interactive (list (current-buffer)))
  (let* ((name (eval-context-make-name "edebug"))
         (buffer (get-buffer name)))
    (unless buffer
      (setq buffer (get-buffer-create name))
      (with-current-buffer buffer
        (funcall (or mode eval-context-default-mode))
        (eval-context-set-edebug)))
  (pop-to-buffer buffer)))

(defun open-repl-in-eval-context-buffer (&optional buffer)
  "Open an IELM repl in the eval-context of BUFFER."
  (interactive (list (current-buffer)))
  (let ((name (eval-context-make-name (buffer-name buffer) t)))
    (save-window-excursion (ielm name)
                           (eval-context-set-buffer buffer))
    (pop-to-buffer name)))

(defun open-repl-in-eval-context-edebug ()
  "Open an IELM repl in the eval-context of edebug."
  (interactive)
  (let ((name (eval-context-make-name "edebug" t)))
    (save-window-excursion (ielm name)
                           (eval-context-set-edebug t))
    (pop-to-buffer name)))


(define-minor-mode eval-context-mode
  "TODO"
  :init-val nil
  :lighter "")

(define-globalized-minor-mode global-eval-context-mode eval-context-mode
  (lambda () (or eval-context-edebug
            (and eval-context-buffer
                 (bufferp eval-context-buffer))))
  "TODO"
  :init-val nil
  :lighter "")


(defadvice! eval-in-context-a (fn &rest args)
  "Advice that allows changing the context an elisp expression is evaluated in."
  :around '(eval-last-sexp org-babel-execute:emacs-lisp ielm-eval-input)
  (let* ((eval-fn (if eval-context-edebug
                      (symbol-function 'edebug-safe-eval)
                    (symbol-function 'eval)))
         (substitute-eval #'(lambda (&rest args)
                            (with-current-buffer (or eval-context-buffer
                                                     (current-buffer))
                              (when eval-context-edebug
                                (setq args (list (car args))))
                              (apply eval-fn args)))))
    (cl-letf (((symbol-function 'eval) substitute-eval)
              ((symbol-function 'edebug-safe-eval) substitute-eval))
      (apply fn args))))


(defun eval-context-update-mode-maybe ()
  "If an eval context is specified turn on eval-context-mode."
  (if (or eval-context-edebug
          eval-context-buffer)
        (eval-context-mode +1)
      (eval-context-mode -1)))

(defun eval-context-buffer-select ()
  "Provide selection UI for selecting a buffer to use as eval-context."
  (let ((prompt (format "Choose context buffer%s: "
                        (if eval-context-buffer
                            (format " (%s)" (buffer-name eval-context-buffer))
                          ""))))
    (completing-read prompt #'internal-complete-buffer)))
