#|
  This file is a part of lQuery
  (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.lib.lquery.test.asd
  (:use :cl :asdf))
(in-package :org.tymoonnext.radiance.lib.lquery.test.asd)

(defsystem lquery-test
  :name "lQuery-Test"
  :version "1.0.0"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "Artistic"
  :description "A library to allow jQuery-like HTML/DOM manipulation. Unit tests package."
  :components ((:file "lquery-test"))
  :depends-on (:lquery
               :fiveam))
