#|
  This file is a part of lQuery
  (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage lquery
  (:nicknames #:org.tymoonnext.radiance.lib.lquery #:radiance-lib-lquery)
  (:use :cl :alexandria :split-sequence)
  (:export
   
   :*lquery-master-document*

   :make-proper-vector
   :copy-proper-vector
   :ensure-proper-vector
   
   :parse-html
   :initialize
   :load-page
   :$
   :inline
   :eval

   :define-node-function
   :define-node-list-function
   :define-value-handler
   :define-argument-handler))

(defpackage lquery-funcs
  (:nicknames #:org.tymoonnext.radiance.lib.lquery.funcs)
  (:export
   :add
   :add-class
   :after
   :ancestor
   :append
   :append-to
   :attr
   :before
   :children
   :child-index
   :clone
   :closest
   :contains
   :contents
   :css
   :data
   :deepest
   :each
   :empty
   :empty-p
   :eq
   :even
   :filter
   :find
   :first
   :gt
   :has
   :has-class
   :hide
   :html
   :html-file
   :index
   :initialize
   :insert-after
   :insert-before
   :is
   :is-empty
   :last
   :length
   :lt
   :map
   :next
   :next-all
   :next-until
   :node
   :not
   :not-empty
   :odd
   :parent
   :parents
   :parents-until
   :prepend
   :prepend-to
   :prev
   :prev-all
   :prev-until
   :remove
   :remove-attr
   :remove-class
   :remove-data
   :replace-all
   :replace-with
   :show
   :siblings
   :size
   :slice
   :text
   :toggle-class
   :unwrap
   :val
   :wrap
   :wrap-all
   :wrap-inner
   :write-to-file
   :serialize))
