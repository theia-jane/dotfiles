(require 'eieio)

;;; Variables
(defvar ob-lob-cache-store-path (concat user-emacs-directory "/ob-lob-cache.el") "")
(defvar ob-lob-cache-store-object nil "")
(defvar ob-lob-cache-enabled nil "")

;;; Helpers
(defun ob-lob-cache-make-item (filename &optional library)
  `(:filename ,filename
    :hash ,(with-temp-buffer
            (insert-file-contents filename)
            (buffer-hash))
    :library ,library))


(defun ob-lob-cache-should-skip-ingest-p (item)
  (let ((cached-item (ob-lob-cache-get-item ob-lob-cache-store-object
                                         (plist-get item :filename))))
    (and cached-item 
         (equal (plist-get cached-item :hash)
                (plist-get item :hash)))))

;;; Advice
(defun ob-lob-cache-advise-org-babel-lob-ingest (ingest-fn filename)
  (if ob-lob-cache-enabled
      (when-let ((item (ob-lob-cache-make-item filename)))
        (if (ob-lob-cache-should-skip-ingest-p item)
            ;; leverage cache
            (let* ((cached-item (ob-lob-cache-get-item ob-lob-cache-store-object filename))
                   (lib (plist-get cached-item :library)))
              ;; Clear out overlappers
              (dolist (entry lib)
                (setq org-babel-library-of-babel
                      (assq-delete-all (car entry) org-babel-library-of-babel)))
              (setq org-babel-library-of-babel
                    (append org-babel-library-of-babel lib)))

          ;; Fill cache
          (let ((old-lob org-babel-library-of-babel))
            ;; Perform ingest
            (funcall ingest-fn filename)
            ;; Extract and save off library additions
            (plist-put item
                       :library
                       (seq-difference org-babel-library-of-babel old-lob))
            ;; Store in cache
            (ob-lob-cache-write-item ob-lob-cache-store-object filename item))))
    ;; Ignore cache
    (funcall ingest-fn filename)))

(advice-add #'org-babel-lob-ingest :around #'ob-lob-cache-advise-org-babel-lob-ingest)

;;; Storage class
(defclass ob-lob-cache-store ()
  ((storage :initarg :storage
            :initform nil)
   (persistance-path :initarg :persistance-path))
  "Storage")

(cl-defmethod ob-lob-cache-persist ((store ob-lob-cache-store))
  (with-temp-buffer
    (insert (pp-to-string (oref store storage)))
    (write-region (point-min) (point-max) (oref store persistance-path))))

(cl-defmethod ob-lob-cache-write-item ((store ob-lob-cache-store) key item)
  (puthash key item (oref store storage))
  (ob-lob-cache-persist store))

(cl-defmethod ob-lob-cache-get-item ((store ob-lob-cache-store) key)
  (gethash key (oref store storage)))

(cl-defmethod ob-lob-cache-load ((store ob-lob-cache-store))
  (when (file-exists-p ob-lob-cache-store-path)
    (with-temp-buffer
      (insert-file-contents (oref store persistance-path))
      (goto-char (point-min))
      (oset store storage (read (current-buffer))))))

(cl-defmethod ob-lob-cache-store-init ((store ob-lob-cache-store))
  (oset store storage (make-hash-table :test 'equal)))

(defun ob-lob-cache-store-init ()
  ;; Make sure dir exists
  (let ((dir (file-name-directory ob-lob-cache-store-path)))
    (unless (file-exists-p dir)
      (make-directory dir t)))

  (setq ob-lob-cache-store-object
        (ob-lob-cache-store :persistance-path ob-lob-cache-store-path
                         :storage (make-hash-table :test 'equal)))

  (ob-lob-cache-load ob-lob-cache-store-object))

(provide 'ob-lob-cache)
;;; ob-lob-cache.el ends here
