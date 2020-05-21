;;; ~/Projects/dotfiles/src/config/doom/+extra-log.el -*- lexical-binding: t; -*-

(defvar +messages-log-file (format "/tmp/emacs-messages-%s.log" (emacs-pid))
  "File to the *Messages* buffer to.")

(defun +message/save-log-file (&rest _)
  "Save the *Messages* buffer to `+messages-log-file'"
  (with-current-buffer (messages-buffer)
    (write-region nil nil +messages-log-file nil -1)))

(defun +message/start-log ()
  "Start logging out the *Messages* buffer to `+messages-log-file'"
  (interactive)
  (+message/stop-log)
  (run-with-timer 1 0.2 '+message/save-log-file))

(defun +message/stop-log ()
  "Stop logging out the *Messages* buffer"
  (interactive)
  (cancel-function-timers '+message/save-log-file))

(defadvice! +log-hooks (fn &rest args)
  "Log hooks that are currently being run.

NOTE: This is still a WIP"
  :around '(run-mode-hooks
            run-hooks
            run-hook-with-args
            run-hook-with-args-until-success
            run-hook-with-args-until-failure
            run-hook-wrapped
            run-window-configuration-change-hook)
  
  (with-current-buffer (+log-hooks-buffer)
    (save-excursion
      (goto-char (point-max))
      (let* ((name (pcase fn
                         ((pred symbolp) (symbol-name fn))
                         ((pred subrp) (subr-name fn))
                         (_ (type-of fn))))
            (run-hooks-name (pcase name
                              ((or
                                "run-hook-wrapped"
                                "run-hook-with-args"
                                "run-hook-with-args-until-success"
                                "run-hook-with-args-until-failure")
                               (list (car args)))
                              ("run-hooks" args)
                              (_ args)))
            (extra-detail ""))
        (insert (concat "\n" (format "[%s] Running hook(s): %s%s"  name run-hooks-name extra-detail)))))
  (apply fn args)))

(defun +log-hooks-buffer () (get-buffer-create (+doom-buffer-name "log-hooks")))
(defun +log-hooks-buffer-clear () (with-current-buffer (+log-hooks-buffer) (erase-buffer)))

(defmacro +message-val (&rest args)
  "Prints out the name of the variable and it's value (if any)."
  `(string-join (mapcar (lambda (symb)
                          (format "%s (%s): %s" (symbol-name symb)
                                  (if (boundp symb)
                                      (type-of (symbol-value symb))
                                    "unbound")
                                  (if (boundp symb)
                                      (symbol-value symb)
                                    ""))
                          ) ',args)
                ", "))
