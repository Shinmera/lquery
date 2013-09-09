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
  :version "2.0.5"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "Artistic"
  :description "A library to allow jQuery-like HTML/DOM manipulation."
  :serial T
  :components ((:file "package")
               (:file "lquery")
               (:file "lquery-funcs"))
  :depends-on (:cxml
               :cxml-dom
               :closure-html
               :css-selectors
               :buildnode
               :alexandria
               :split-sequence))
