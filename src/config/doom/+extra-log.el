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
