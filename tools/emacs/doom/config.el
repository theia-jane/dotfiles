;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Theia Ware"
      user-mail-address "mail@theia-jane.dev")

(setq doom-leader-key "H-SPC"
      doom-leader-alt-key "H-SPC"
      doom-localleader-key "H-m"
      doom-localleader-alt-key "H-m")


;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-flatwhite)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(use-package! obsidian
  :config
  (global-obsidian-mode t)
  :custom
  (obsidian-daily-notes-directory "Dailies")
  (obsidian-directory "~/Documents/Notes"))


(after! (doom)
  (require 'which-key)
  (defun ~remap-leader-prefix-keys-a (&rest _)

    (require 'timeout)
    (duplicate-prefix-key
     "H-SPC b" "H-b"
     "H-SPC f" "H-f"
     "H-SPC n" "H-n"
     "H-SPC g" "H-g"
     "H-SPC p" "H-p"
     "H-SPC s" "H-/"))

  (timeout-debounce! '~remap-leader-prefix-keys-a 0.3)
  (advice-add 'general-define-key :after '~remap-leader-prefix-keys-a)
  (advice-add 'map! :after '~remap-leader-prefix-keys-a)
  (~remap-leader-prefix-keys-a))


(defvar notes-dir "~/Desktop/Notes")
(setq org-directory notes-dir)
(defvar periodic-notes-subdir "Periodic")

(defun periodic-increment-by (type n &optional time)
  (let* ((time (or time (current-time)))
         (decoded (decode-time time))
         (decoded_month (nth 4 decoded))
         (decoded_year (nth 5 decoded)))
    (pcase type
      ('day (time-add time (seconds-to-time (* n 24 60 60))))
      ('month (progn
               (calendar-increment-month decoded_month decoded_year n)
               (setf (nth 4 decoded) decoded_month
                     (nth 5 decoded) decoded_year)
               (encode-time decoded)))
      ('year (progn
              (setf (nth 5 decoded) (+ n decoded_year))
              (encode-time decoded))))))

(defun periodic-notes-file-path (type &optional time)
  (let* ((format-path
         (pcase type
           ('year "%Y/year.org")
           ('month "%Y/%m/month.org")
           ('day "%Y/%m/%d.org")))
         (time
          (pcase time
            ('next (periodic-increment-by type 1))
            ('prev (periodic-increment-by type -1))
            (_ time))))
    (f-join
     notes-dir
     periodic-notes-subdir
        (format-time-string format-path time))))

(defun periodic-goto (type &optional time)
  (let ((filepath (periodic-notes-file-path type time)))
    (unless (file-exists-p filepath)
      (let ((dir (file-name-directory filepath)))
        (unless (file-exists-p dir)
          (make-directory dir t))))
        (find-file filepath)))

(defun periodic-goto-daily (&optional time)
  (interactive)
  (periodic-goto 'day time))

(defun periodic-goto-monthly (&optional time)
  (interactive)
  (periodic-goto 'month time))

(defun periodic-goto-yearly (&optional time)
  (interactive)
  (periodic-goto 'year time))


(after! projectile
  (add-to-list 'projectile-project-root-files ".project"))

(map! :leader
      :prefix "n"
      (:prefix-map ("p" . "periodic")
       :desc "Daily note"    "d" #'periodic-goto-daily
       :desc "Monthly note"  "m" #'periodic-goto-monthly
       :desc "Yearly note"   "y" #'periodic-goto-yearly
       (:prefix-map ("p" . "previous")
        :desc "Yesterday's note"   "d" (cmd! (periodic-goto-daily 'prev))
        :desc "Last month's note"  "m" (cmd! (periodic-goto-monthly 'prev))
        :desc "Last year's note"   "y" (cmd! (periodic-goto-yearly 'prev)))
       (:prefix-map ("n" . "next")
        :desc "Tomorrow's note"    "d" (cmd! (periodic-goto-daily 'next))
        :desc "Next month's note"  "m" (cmd! (periodic-goto-monthly 'next))
        :desc "Next year's note"   "y" (cmd! (periodic-goto-yearly 'next)))
       )
  )

(map! :map global-mode-map "H-s" #'yas-next-field-or-maybe-expand)

(map! :leader :prefix "s" :desc "Search project" "/" #'+default/search-project)




(defalias 'keymap! 'map!)

(require 'package-utils)
(require 'personal-lib)
(require 'config-evil)
(require 'config-elisp)
(require 'config-ui-themes)
(require 'config-org)
(require 'config-org-ui)
(require 'config-org-babel)
(require 'config-org-latex)
(require 'config-org-export)
(require 'config-org-links)
(require 'config-file-management)
