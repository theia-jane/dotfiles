;;; -*- lexical-binding: t; -*-

(provide 'color-lib)

(require 'color)
(require 'dash)

(defun hex-to-rgb (hex)
  (when (equal "#" (substring hex 0 1))
    (setq hex (substring hex 1)))
  (let ((red (substring hex 0 2))
        (green (substring hex 2 4))
        (blue (substring hex 4 6)))
    (mapcar
     (-compose
      (lambda (num)
        (/ (float num) 255))
      (-rpartial #'string-to-number 16))
      (list red green blue))))

(defun hex-to-hsl (hex)
  (apply #'color-rgb-to-hsl
         (hex-to-rgb hex)))

(defun hex-to-hsl (hex)
  (apply #'color-rgb-to-hsl
         (hex-to-rgb hex)))

(defun hsl-to-hex (hsl)
  (apply (-rpartial #'color-rgb-to-hex 2)
    (apply #'color-hsl-to-rgb hsl)))

(defun hex-to-hsv (hex)
  (apply #'color-rgb-to-hsl
         (hex-to-rgb hex)))

(defun hex-lighten (hex percent)
  (hsl-to-hex
   (apply (-rpartial #'color-lighten-hsl percent)
           (hex-to-hsl hex))))

(defun hex-darken (hex percent)
  (hsl-to-hex
   (apply (-rpartial #'color-darken-hsl percent)
           (hex-to-hsl hex))))

(defun hex-saturate (hex percent)
  (hsl-to-hex
   (apply (-rpartial #'color-saturate-hsl percent)
           (hex-to-hsl hex))))

(defun hex-desaturate (hex percent)
  (hsl-to-hex
   (apply (-rpartial #'color-desaturate-hsl percent)
           (hex-to-hsl hex))))
