;;; -*- lexical-binding: t; -*-
(provide 'eval-context)
(require 'personal-lib)

(defvar eval-context-default-mode 'emacs-lisp-mode)

(defvar-local eval-context-buffer nil "TODO")
(put 'eval-context-buffer 'permanent-local t)

(defvar-local eval-context-edebug nil "TODO")
(put 'eval-context-edebug 'permanent-local t)

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
  ""
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

(defun eval-context-buffer-buffer (&optional context-buffer mode)
  "TODO"
  (interactive (list (current-buffer)))
  (let* ((name (format "*eval-context[%s]*" (buffer-name context-buffer)))
         (buffer (get-buffer name)))
    (unless buffer
      (setq buffer (get-buffer-create name))
      (with-current-buffer buffer
        (funcall (or mode eval-context-default-mode))
        (eval-context-set-buffer context-buffer)))
  (pop-to-buffer buffer)))

(defun eval-context-edebug-buffer (&optional mode)
  "TODO"
  (interactive (list (current-buffer)))
  (let* ((name "*eval-context[edebug]*")
         (buffer (get-buffer name)))
    (unless buffer
      (setq buffer (get-buffer-create name))
      (with-current-buffer buffer
        (funcall (or mode eval-context-default-mode))
        (eval-context-set-buffer context-buffer)))
  (pop-to-buffer buffer)))

(defun eval-context-set-buffer (&optional buffer)
  (interactive (list (eval-context-buffer-select)))
  (setq eval-context-buffer (and buffer (get-buffer buffer)))
  (eval-context-update-mode-maybe))

(defun eval-context-update-mode-maybe ()
  (if (or eval-context-edebug
          eval-context-buffer)
        (eval-context-mode +1)
      (eval-context-mode -1)))

(defun eval-context-set-edebug (&optional arg)
  (interactive (list 1))
  (setq eval-context-edebug arg)
  (eval-context-update-mode-maybe))

(defun eval-context-buffer-select ()
  (let ((prompt (format "Choose context buffer%s: "
                        (if eval-context-buffer
                            (format " (%s)" (buffer-name eval-context-buffer))
                          ""))))
    (completing-read prompt #'internal-complete-buffer)))

(defun open-elisp-repl-in-eval-context (&optional buffer)
  (interactive (list (current-buffer)))
  (ielm)
  (eval-context-set-buffer buffer))
