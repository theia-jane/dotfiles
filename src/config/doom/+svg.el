;;; ~/Projects/dotfiles/src/config/doom/+svg.el -*- lexical-binding: t; -*-

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
                                 (size "500px"))
  (svg:
   :xmlns "http://www.w3.org/2000/svg"
   :xmlns:xlink "http://www.w3.org/1999/xlink"
   :width size
   :height size
   :viewBox "0 0 2304 2304"
   :children
   (svg:defs)
   (svg:path :id "outer-box-shell"
             :transform "matrix(0 1.74762982835482 -1.74762982835482 0 2085.11981267341 72.5429140623223)"
             :fill outer-box
             :d "M308.718 0.120918L925.516 0L1234.26 533.348L925.363 1066.7L308.718 1066.7L0 533.348Z")
   (svg:path :id "outer-box-edge-shadow"
             :transform "matrix(2.96482102905814 0 0 2.96482102905814 1149.96359954899 611.433658908337)"
             :fill (hex-darken outer-box 3)
             :d "M314.859 0L0 183.6L4.54747e-13 546.295L315.222 363.614Z")
   (svg:path :id "outer-box-cutout"
             :transform "matrix(0 -1.47493865343202 1.47493865343202 0 366.36281618265 2062.6954133316)"
             :fill outer-box
             :d "M308.718 0.120918L925.516 0L1234.26 533.348L925.363 1066.7L308.718 1066.7L0 533.348Z")
   (svg:path :id "outer-box-wall-shadow"
             :transform "matrix(-2.50220559601143 0 0 -2.50220559601143 1155.60206087599 1607.89041020437)"
             :fill (hex-darken outer-box 3)
             :d "M314.859 0L0 183.6L4.54747e-13 546.295L315.222 363.614Z")
   (svg:path :id "inner-box"
             :transform "matrix(0 0.589455427231726 -0.589455427231726 0 1467.41744427774 786.638373243171)"
             :fill inner-box
             :d "M308.718 0.120918L925.516 0L1234.26 533.348L925.363 1066.7L308.718 1066.7L0 533.348Z")
   (svg:path :id "inner-box-shadow"
             :transform "matrix(1.00000003993522 0 0 1.00000003993522 1152.00004600538 968.400057256455)"
             :fill (hex-desaturate
                    (hex-darken inner-box 15)
                    20)
             :d "M314.859 0L0 183.6L4.54747e-13 546.295L315.222 363.614Z")))


(cl-defun doom/wayward-arcanist-logo (&key (size "500"))
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
