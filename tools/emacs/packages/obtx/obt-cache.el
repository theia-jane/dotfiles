(require 'eieio)


;;; Variables
(defvar obt-cache-store-path
  (concat user-emacs-directory "/obt-cache-record.el")
  "")
(defvar obt-cache-store-object nil "")
(defvar obt-cache-enabled nil "")

;;; Helpers
(defun obt-cache-make-item (tangle-args &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (when-let ((filename (buffer-file-name)))
      `(:filename ,filename
        :hash ,(buffer-hash)
        :tangle-sig ,(sha1 (pp-to-string tangle-args))))))


(defun obt-cache-should-skip-tangle-p (item)
  (let ((cached-item (obt-cache-get-item obt-cache-store-object
                                         (plist-get item :filename))))
    (and cached-item 
         (equal (plist-get cached-item :hash)
                (plist-get item :hash))
         (equal (plist-get cached-item :tangle-sig)
                (plist-get item :tangle-sig)))))

;;; Advice
(defun obt-cache-advise-org-babel-tangle (tangle-fn &rest tangle-args)
  (if obt-cache-enabled
      (when-let ((item (obt-cache-make-item tangle-args)))
        (unless (obt-cache-should-skip-tangle-p item)
          (apply tangle-fn tangle-args)
          (obt-cache-write-item obt-cache-store-object
                                (plist-get item :filename)
                                item)))
      (apply tangle-fn tangle-args)))

(advice-add #'org-babel-tangle :around #'obt-cache-advise-org-babel-tangle)

;;; Storage class
(defclass obt-cache-store ()
  ((storage :initarg :storage
            :initform nil)
   (persistance-path :initarg :persistance-path))
  "Storage")

(cl-defmethod obt-cache-persist ((store obt-cache-store))
  (with-temp-buffer
    (insert (pp-to-string (oref store storage)))
    (write-region (point-min) (point-max) (oref store persistance-path))))

(cl-defmethod obt-cache-write-item ((store obt-cache-store) key item)
  (puthash key item (oref store storage))
  (obt-cache-persist store))

(cl-defmethod obt-cache-get-item ((store obt-cache-store) key)
  (gethash key (oref store storage)))

(cl-defmethod obt-cache-load ((store obt-cache-store))
  (when (file-exists-p obt-cache-store-path)
    (with-temp-buffer
      (insert-file-contents (oref store persistance-path))
      (goto-char (point-min))
      (oset store storage (read (current-buffer))))))

(cl-defmethod obt-cache-store-init ((store obt-cache-store))
  (oset store storage (make-hash-table :test 'equal)))

(defun obt-cache-store-init ()
  ;; Make sure dir exists
  (let ((dir (file-name-directory obt-cache-store-path)))
    (unless (file-exists-p dir)
      (make-directory dir t)))

  (setq obt-cache-store-object
        (obt-cache-store :persistance-path obt-cache-store-path
                         :storage (make-hash-table :test 'equal)))

  (obt-cache-load obt-cache-store-object))

(provide 'obt-cache)
;;; obt-cache.el ends here
