;;; ui/dashboard/config.el -*- lexical-binding: t; -*-


(defvar dashboard-definitions-alist nil "TODO")

(defun dashboard-name (type)
  "TODO"
  (+doom-buffer-name (symbol-name type)))

(defun dashboard--render (type)
  "TODO"
  (when-let* ((buffer (get-buffer-create (dashboard-name type)))
              (dashboard-definition (cdr (assq type dashboard-definitions-alist))))
    (let ((renders (plist-get dashboard-definition :renders)))
      (seq-do `(lambda (x) (message "%s" x)) renders)
      (with-current-buffer buffer
        (with-silent-modifications
          (let ((pt (point)))
            (unless (eq major-mode '+doom-dashboard-mode)
              (+doom-dashboard-mode))
            (setq-local dashboard--type type)
            (erase-buffer)
            (seq-do #'funcall renders)
            (goto-char pt))
          (current-buffer))))))

(defun dashboard--open (type)
  "TODO"
  (switch-to-buffer (dashboard--render type)))

(cl-defmacro define-dashboard! (type &keys renders)
  "TODO"
  (setq dashboard-definitions-alist (assq-delete-all type dashboard-definitions-alist))
  (push `(,type :renders ,(doom-unquote renders)) dashboard-definitions-alist)
  `(progn
     (defun ,(intern (format "%s-dashboard-render" type)) ()
       (dashboard--render ',type))

     (defun ,(intern (format "%s-dashboard" type)) ()
       (interactive)
       (dashboard--open ',type))))

;; (define-dashboard! testing
;;   :renders '((lambda ()
;;              (insert
;;               (concat "\n"
;;                       (dashboard-menu
;;                        +doom-dashboard-menu-sections
;;                        nil
;;                        t
;;                        'left)
;;                       "\n")))))

;; ui components
(defun dashboard--center (len string)
  (map-lines #'(lambda (line)
                 (concat
                  (make-string (ceiling (max 0 (- len (length lines))) 2) ? ) lines)) string))

(defun dashboard-list-seperator (&optional compact)
  (if (and (not compact) (display-graphic-p))
      "\n\n"
    "\n"))

(defun dashboard-menu (menu-items &optional show-key compact alignment)
  "TODO"
  (string-box-format
   (mapconcat #'(lambda (section)
                  (dashboard-menu-item section show-key (eq alignment 'right)))
              menu-items
              (dashboard-list-seperator compact)) alignment))

(defun dashboard-menu-item (section &optional show-key right-align)
  (cl-destructuring-bind (label &key icon action when face) section
    (when (and (fboundp action)
               (or (null when)
                   (eval when t)))
      (let ((menu-item-parts (list (dashboard-icon icon t)
                                   (dashboard-button label action face)
                                   ;; TODO (move this up a level)
                                   (if show-key
                                       (format "%-10s" (propertize (dashboard--get-action-key action) 'face 'doom-dashboard-menu-desc))
                                     "")))
            (format-pattern (if right-align "%10s%s%s" "%s%s%-10s")))
        (apply 'format format-pattern
               (if right-align
                   (reverse menu-item-parts)
                 menu-item-parts)
               )))))

(defun dashboard-icon (icon &optional fill-space)
  (let* ((all-the-icons-scale-factor 1.45)
         (all-the-icons-default-adjust -0.02)
         (icon (if (stringp icon) icon (eval icon t))))
    (cond (icon (format "%-3s" icon))
          (fill-space (format "%-3s" ""))
          (t ""))))

(defun dashboard-button(label action face)
  (with-temp-buffer
    (insert-text-button
     label
     'action
     `(lambda (_)
        (call-interactively (or (command-remapping #',action)
                                #',action)))
     'face (or face 'doom-dashboard-menu-title)
     'follow-link t
     'help-echo
     (format "%s (%s)" label
             (propertize (symbol-name action) 'face 'doom-dashboard-menu-desc)))
    (buffer-string)))

;;; Helpers
(defun dashboard--get-action-key (action)
  "Lookup command keys dynamically"
  (or (when-let (key (where-is-internal action nil t))
        (with-temp-buffer
          (save-excursion (insert (key-description key)))
          (while (re-search-forward "<\\([^>]+\\)>" nil t)
            (let ((str (match-string 1)))
              (replace-match
               (upcase (if (< (length str) 3)
                           str
                         (substring str 0 3))))))
          (buffer-string)))
      ""))

(defun string-box-format (contents &optional alignment)
  (let* ((lines (split-string contents "\n"))
         (max-line-length (apply #'max (mapcar #'length lines)))
         (format-pattern (format "%%%s%ss"
                                 (if (eq alignment 'right) "" "-")
                                 max-line-length)))
    (mapconcat #'(lambda (line) (format format-pattern line)) lines "\n")))


(defun map-lines (fn string)
  (mapconcat fn (split-string string "\n") "\n"))
