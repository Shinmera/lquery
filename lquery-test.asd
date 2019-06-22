#|
  This file is a part of lQuery
  (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem lquery-test
  :name "lQuery-Test"
  :version "1.0.0"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "A library to allow jQuery-like HTML/DOM manipulation. Unit tests package."
  :components ((:file "lquery-test"))
  :depends-on (:lquery
               :fiveam))
