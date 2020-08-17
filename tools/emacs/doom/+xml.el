;;; ~/Projects/dotfiles/src/config/doom/+xml.el -*- lexical-binding: t; -*-

(require 'xml)
(defun xml:attributes (&rest args)
  "Produce xml attributes from ARGS which are plist-like.

Everything that follows one keyword will assigned to it, until the next
keyword is arrived at. Example:

    (xml:attributes :one 1 :two 2 3 :three 4)

will return:

    one=\"1\" two=\"2 3\" three=\"4\"
"
  (let (attributes
        arg
        attribute)
    (while args
      (setq arg (pop args))
      (cond
       ((keywordp arg)
        (when attribute
          (push attribute attributes))
        (setq attribute
              (list
               (substring (symbol-name arg) 1))))
       (t (setq attribute (append attribute
                                  (list arg))))))

    (when attribute
      (push attribute attributes))

    (mapconcat
     (lambda (attribute)
       (let ((attribute-string (car attribute)))
         (when (cdr attribute)
           (setq attribute-string
                 (format "%s=\"%s\""
                         attribute-string
                         (xml-escape-string
                          (s-join " " (cdr attribute))))))
         attribute-string))
     (reverse attributes)
     " ")))

(defun xml:tag (type &rest args)
  "Generate an xml tag with TYPE and ARGS.

TYPE is a symbol.

ARGS is plist-like. The `:children' key marks the children
elements of the tag - all elements that follow this key are
considered the children of the tag. All other keywords are
processed as xml attributes and passed to `xml:attributes'.
"
  (let* ((tag-name (symbol-name type))
         (children-index (-elem-index :children args))
         (children (when children-index
                     (cl-subseq args (1+ children-index))))
         (attributes (if children-index
                         (cl-subseq args 0 children-index)
                         args)))
    (concat "<" tag-name
            " "
            (apply #'xml:attributes attributes)
            " "
            (if children ">" "/>")
            (when children
              (concat
               "\n"
                (s-join "\n" children)
                "\n"
                (concat "</" tag-name ">"))))))

(defun xml:comment (comment)
  "Generate a xml COMMENT."
  (concat "<!-- "
          comment
          "-->"))
