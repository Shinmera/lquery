#|
  This file is a part of lQuery
  (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem lquery
  :name "lQuery"
  :version "3.2.1"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "A library to allow jQuery-like HTML/DOM manipulation."
  :homepage "https://Shinmera.github.io/lquery/"
  :bug-tracker "https://github.com/Shinmera/lquery/issues"
  :source-control (:git "https://github.com/Shinmera/lquery.git")
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
