#|
  This file is a part of lQuery
  (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem lquery
  :name "lQuery"
  :version "3.2.0"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "Artistic"
  :description "A library to allow jQuery-like HTML/DOM manipulation."
  :homepage "https://github.com/Shinmera/lquery"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "lquery")
               (:file "lquery-funcs")
               (:file "lquery-macros"))
  :depends-on (:array-utils
               :form-fiddle
               :plump
               :clss)
  :in-order-to ((asdf:test-op (asdf:test-op :lquery-test))))
