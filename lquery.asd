#|
  This file is a part of lQuery
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.lib.lquery.asd
  (:use :cl :asdf))
(in-package :org.tymoonnext.radiance.lib.lquery.asd)

(defsystem lquery
  :name "lQuery"
  :version "1.0.3"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "LLGPL"
  :description "A library to allow jQuery-like HTML/DOM manipulation."
  :components ((:file "lquery")
               (:file "lquery-funcs" :depends-on ("lquery")))
  :depends-on (:cxml
               :cxml-dom
               :closure-html
               :css-selectors
               :buildnode
               :alexandria
               :split-sequence))
