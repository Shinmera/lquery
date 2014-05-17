#|
  This file is a part of lQuery
  (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :cl-user)
(defpackage :org.tymoonnext.lquery.doc
  (:use :cl :lquery :lquery-doc)
  (:nicknames :lquery-documentation)
  (:export :build-documentation))

(in-package :org.tymoonnext.lquery.doc)

(defmethod documentate-object :after (template object fields)
  ($ template ".anchor" (attr "name" (symbol-name (nth 0 object))))
  ($ template "h3 a" (attr "href" (format NIL "#~a" (symbol-name (nth 0 object))))))

(defun build-documentation ()
  ($ (initialize (merge-pathnames "about-template.html" (asdf:system-source-directory :lquery))))
  (let ((template ($ "#template")))
    (let ((nodes (lquery-doc::documentate template :lquery :exclude '(:internal :method))))
      ($ "#docs" (empty) (append nodes)))
    (let ((nodes (lquery-doc::documentate template :lquery-funcs :exclude '(:internal :method))))
      ($ "#func-docs" (empty) (append nodes))))
  ($ (write-to-file (merge-pathnames "about.html" (asdf:system-source-directory :lquery)))))
