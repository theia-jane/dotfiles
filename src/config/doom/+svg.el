;;; ~/Projects/dotfiles/src/config/doom/+svg.el -*- lexical-binding: t; -*-

(load! "+xml.el")

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
  "Create a svg defs tag with ARGS.

For valid definitions of ARGS see `xml:tag'.
"
  (apply
   #'xml:tag
   'defs
   args))

(defun svg:path (&rest args)
  "Create a svg path tag with ARGS.

For valid definitions of ARGS see `xml:tag'.
"
  (apply
   #'xml:tag
   'path
   args))


(defmacro comment (&rest _) nil)
(comment
Logo I made:
(svg:
 :viewBox "0 0 2304 2304"
 :children
 (svg:defs)
 (svg:path :id "shape0"
           :transform "matrix(0 1.00510961037428 -1.00510961037428 0 2047.67993897439 86.3999974250796)"
           :fill "#efecca"
           :fill-rule "evenodd"
           :d "M530.622 0.201729L1590.77 0L2121.43 889.792L1590.51 1779.59L530.622 1779.59L0 889.792Z")

 (svg:path :id "shape01"
           :transform "matrix(0 1.74762982835482 -1.74762982835482 0 2085.11981267341 72.5429140623223)"
           :fill "#3c3843"
           :fill-rule "evenodd"
           :d "M308.718 0.120918L925.516 0L1234.26 533.348L925.363 1066.7L308.718 1066.7L0 533.348Z")

 (svg:path :id "shape011"
           :transform "matrix(2.96482102905814 0 0 2.96482102905814 1149.96359954899 611.433658908337)"
           :fill "#35313b"
           :fill-rule "evenodd"
           :d "M314.859 0L0 183.6L4.54747e-13 546.295L315.222 363.614Z")
 (svg:path :id "shape011"
           :transform "matrix(0 -1.47493865343202 1.47493865343202 0 366.36281618265 2062.6954133316)"
           :fill "#3c3843"
           :fill-rule "evenodd"
           :d "M308.718 0.120918L925.516 0L1234.26 533.348L925.363 1066.7L308.718 1066.7L0 533.348Z")
 (svg:path :id "shape01"
           :transform "matrix(-2.50220559601143 0 0 -2.50220559601143 1155.60206087599 1607.89041020437)"
           :fill "#35313b"
           :fill-rule "evenodd"
           :d "M314.859 0L0 183.6L4.54747e-13 546.295L315.222 363.614Z")
 (svg:path :id "shape012"
           :transform "matrix(0 0.589455427231726 -0.589455427231726 0 1467.41744427774 786.638373243171)"
           :fill "#efecca"
           :fill-rule "evenodd"
           :stroke "#efecca"
           :stroke-width "1"
           :stroke-linecap "square"
           :stroke-linejoin "bevel"
           :d "M308.718 0.120918L925.516 0L1234.26 533.348L925.363 1066.7L308.718 1066.7L0 533.348Z")
 (svg:path :id "shape02"
           :transform "matrix(1.00000003993522 0 0 1.00000003993522 1152.00004600538 968.400057256455)"
           :fill "#d0ccaf"
           :fill-rule "evenodd"
           :d "M314.859 0L0 183.6L4.54747e-13 546.295L315.222 363.614Z")))
