;;; ~/Projects/dotfiles/src/config/doom/+svg.el -*- lexical-binding: t; -*-

(require 'dash-functional)
(require 's)

(load! "+xml.el")
(load! "+color.el")

(defun svg: (&rest args)
  "Create a svg string using ARGS.

For valid definitions of ARGS see `xml:tag'.
"
  (concat
   "<?xml "
   (xml:attributes
    :version "1.0"
    :standalone "no")
   " ?>\n"
   "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 20010904//EN\" \"http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd\">\n"
  (xml:comment "Created using emacs!")
  (apply
   #'xml:tag
   'svg
   args)))

(defun svg:defs (&rest args)
  (apply
    #'xml:tag
    'defs
    args))

(defun svg:path (&rest args)
  (apply
    #'xml:tag
    'path
    args))

(defmacro comment (&rest _) nil)

(defun svg/as-image (svg)
  (create-image svg 'svg t))

(defun svg/preview-in-buffer (svg)
  (let* ((buffer-name "*svg-preview*")
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert-image (svg/as-image svg)))
    (+buffer-open-in-vertical-split-maybe buffer)))

(cl-defun wayward-arcanist-logo (&key
                                 (outer-box "#3c3843")
                                 (inner-box "#efecca")
                                 (size 500))
  (let* ((canvas-offset (- (/ (float size) 2)))
        (outer-side (* (/ (float 2) 5) size))
        (outer-depth (* (/ (float 1) 10) size))
        (inside-edge (- outer-side outer-depth))
        (inner-side (* (/ (float 1) 10) size))
        (dx (cos (/ pi 6)))
        (dy (sin (/ pi 6))))
  (svg:
   :xmlns "http://www.w3.org/2000/svg"
   :xmlns:xlink "http://www.w3.org/1999/xlink"
   :width (number-to-string size)
   :height (number-to-string size)
   :viewBox (mapconcat
             #'number-to-string
             (list canvas-offset canvas-offset
                   size size)
             " ")
   :children
   (svg:defs)
   (svg:path :d (svg:path-hexagon outer-side)
             :transform (svg:transform :rotate 90)
             :fill outer-box)
   (svg:path :fill (hex-darken outer-box 3)
             :d (svg:path-define
                   :move 0 (+ (/ (float inside-edge) 2) (* inside-edge dy))
                   :line 0 outer-depth
                   :line (* outer-side dx) (- (* outer-side dy))
                   :line 0 (- outer-side)
                   :line (- (* outer-depth dx)) (* outer-depth dy)
                   :line 0 inside-edge
                   :close))
   (svg:path :fill (hex-darken outer-box 3)
             :d (svg:path-define
              :move 0 0
              :line 0 (- inside-edge)
              :line (- (* inside-edge dx)) (* inside-edge dy)
              :line 0 inside-edge
              :close))
   (svg:path :d (svg:path-hexagon inner-side)
             :fill inner-box
             :transform (svg:transform
                         :rotate 90))
   (svg:path :d (svg:path-define
                 :move 0 0
                 :line 0 inner-side
                 :line (* inner-side dx) (- (* inner-side dy))
                 :line 0 (- inner-side)
                 :close)
             :fill (hex-desaturate
                    (hex-darken inner-box 15) 20)))))

(cl-defun doom/wayward-arcanist-logo (&key (size 500))
  (wayward-arcanist-logo :size size
                         :outer-box "#32302f"
                         :inner-box "#d3869b"))

(defun svg:transform (&rest args)
  (let (arg
        transform
        transforms
        commit-fn)
    (while args
      (let* ((arg (pop args))
             (next-arg (car args))
             (value (if (keywordp arg)
                        (substring (symbol-name arg) 1)
                    arg)))

        (setq transform (append transform
                                (list value)))
        (when (and transform
                   (or (null args)
                       (keywordp next-arg)))
          (push (concat
                 (car transform)
                 (when (cdr transform)
                   (prin1-to-string (cdr transform))))
                transforms)
          (setq transform nil))))
    (s-join "\n" (reverse transforms))))

(defconst svg-path-define-commands
  '(:move "m"
    :Move "M"
    :line "l"
    :Line "L"
    :h-line "h"
    :H-line "H"
    :v-line "v"
    :V-line "V"
    :cubic-bezier "c"
    :Cubic-bezier "C"
    :smooth-cubic-bezier "s"
    :Smooth-cubic-bezier "S"
    :quad-bezier "q"
    :Quad-bezier "Q"
    :smooth-quad-bezier "t"
    :Smooth-quad-bezier "T"
    :arc "a"
    :Arc "A"
    :close "z"
    :Close "Z"))

(defun svg:path-define (&rest args)
  (let (arg
        transform
        transforms
        commit-fn)
    (while args
      (let* ((arg (pop args))
             (next-arg (car args))
             (value (if (and (keywordp arg)
                             (plist-get svg-path-define-commands arg))
                        (plist-get svg-path-define-commands arg)
                      arg)))

        (setq transform (append transform
                                (list value)))
        (when (and transform
                   (or (null args)
                       (keywordp next-arg)))

          (push (concat (car transform) " "
                 (mapconcat #'prin1-to-string (cdr transform) " "))
                transforms)
          (setq transform nil))))
    (s-join "\n" (reverse transforms))))

(defun svg:path-hexagon (length)
  (let ((dx (* length (sin (/ pi 6))))
        (dy (* length (cos (/ pi 6)))))
    (svg:path-define
     :move (- (/ (float length) 2)) (- dy)
     :line length 0
     :line dx dy
     :line (- dx) dy
     :line (- length) 0
     :line (- dx) (- dy)
     :line dx (- dy)
     :close)))

