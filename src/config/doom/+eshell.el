;;; ~/Projects/dotfiles/src/config/doom/+eshell.el -*- lexical-binding: t; -*-


(defun +eshell-get-contextual-buffer ()
  (+ensure-buffer
   (+doom-buffer-name "eshell")
   '(lambda () (eshell-mode))))

(defun +eshell-cd-silently (dir)
  (eshell/cd dir)
  (save-excursion
    (let ((inhibit-read-only t)
          (end-point (line-end-position)))
      (forward-line -2)
      (end-of-line)
      (delete-region (point) end-point)))
  (eshell-reset))

(defun +eshell-run-command-visibly (command &optional dir)
  (let ((buffer (+eshell-get-contextual-buffer)))
    (with-current-buffer buffer
      (when dir
        (+eshell-cd-silently dir))
      (+eshell-run-command command)
      (evil-insert-state))
    (unless (get-buffer-window buffer)
      (pop-to-buffer buffer))))


;; (defun +run/popup (&rest args)
;;   (+run--exec args t))

;; (defun +run/here (&rest args)
;;   (+run--exec args nil))

;; (defun +run--exec (args &optional popup-p)
;;   "Run the specified PROGRAM in a terminal emulation buffer.
;; ARGS are passed to the program.  At the moment, no piping of input is
;; allowed."
;;   (require 'em-term)
;;   (let* ((interp (eshell-find-interpreter (car args) (cdr args) t))
;;          (program (car interp))
;;          (args (append (cdr interp) (cdr args)))
;;          (term-buffer-name (concat "*+run:"
;;                               (if popup-p "popup:")
;;                               (file-name-nondirectory program)
;;                               "*"))
;;          (term-buffer (progn
;;                         (when (get-buffer term-buffer-name)
;;                           (kill-buffer term-buffer-name))
;;                         (generate-new-buffer term-buffer-name))))
;;     (save-selected-window
;;       (if popup-p
;;           (display-buffer term-buffer)
;;         (switch-to-buffer term-buffer))

;;       (with-current-buffer term-buffer
;;         (term-mode)
;;         (set (make-local-variable 'term-term-name) eshell-term-name)
;;         (term-exec term-buffer program program nil args)
;;         (let ((proc (get-buffer-process term-buffer)))
;;           (if (and proc (eq 'run (process-status proc)))
;;               (set-process-sentinel proc #'+run--sentinel)
;;             (error "Failed to invoke visual command")))
;;         (term-char-mode)
;;         (+run--register-keys)
;;         (term-set-escape-char ?\C-x)))
;;     )
;;   nil)

;; (defun +run--register-keys ()
;;   (map! :map local
;;         :desc "Kill buffer" "q" 'kill-current-buffer
;;         :desc "Kill process" "C-c" 'term-kill-subjob))

;; (defun +run--sentinel (proc msg)
;;   (term-sentinel proc msg)
;;   (let ((buffer (process-buffer proc)))
;;     (when (and (memq (process-status proc) '(signal exit))
;;                (not (null (buffer-name buffer))))
;;       (with-current-buffer buffer
;;         (let ((buffer-read-only nil))
;;           (evil-normal-state)
;;           (goto-char (point-max)))))

;;     (when (and buffer (buffer-live-p buffer)
;;                (not (eq 'run (process-status proc)))
;;                (= (process-exit-status proc) 0))
;;       (goto-char (point-max)))))


;; (set-popup-rule! "^\\*\\+run:popup:" :size 0.35)
