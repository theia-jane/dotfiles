;;; process-helpers.el --- Description -*- lexical-binding: t; -*-
(defun process-running-p (p)
  (equal (process-status p) 'run))

(defun at-least-one-process-running-p ()
  (seq-some #'process-running-p (process-list)))

(defun wait-for-process (p)
  (while (process-running-p p) 
    (sleep-for 1)))

(defun wait-for-processes ()
  (while (at-least-one-process-running-p) 
    (sleep-for 1)))

(provide 'process-helpers)
;;; process-helpers.el ends here
