(asdf:defsystem lquery-test
  :name "lQuery-Test"
  :version "1.0.0"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "A library to allow jQuery-like HTML/DOM manipulation. Unit tests package."
  :components ((:file "lquery-test"))
  :depends-on (:lquery
               :fiveam))
