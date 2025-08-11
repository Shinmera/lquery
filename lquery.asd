(asdf:defsystem lquery
  :name "lQuery"
  :version "3.2.1"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "A library to allow jQuery-like HTML/DOM manipulation."
  :homepage "https://shinmera.com/docs/lquery/"
  :bug-tracker "https://shinmera.com/project/lquery/issues"
  :source-control (:git "https://shinmera.com/project/lquery.git")
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
