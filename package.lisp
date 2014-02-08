#|
  This file is a part of lQuery
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.lib.lquery
  (:nicknames :lquery :radiance-lib-lquery)
  (:use :cl :css :buildnode :alexandria :split-sequence)
  (:export
   
   :*lquery-master-document*
   :parse-html
   :initialize
   :load-page
   :$
   :inline

   :define-node-function
   :define-node-list-function
   :define-symbol-handler
   :define-argument-handler))

(defpackage org.tymoonnext.radiance.lib.lquery.funcs
  (:use :cl :lquery :buildnode :css :alexandria :split-sequence)
  (:nicknames :lquery-funcs)
  (:export
   :nodefun-add
   :nodefun-add-class
   :nodefun-after
   :nodefun-ancestor
   :nodefun-append
   :nodefun-append-to
   :nodefun-attr
   :nodefun-before
   :nodefun-children
   :nodefun-child-index
   :nodefun-clone
   :nodefun-closest
   :nodefun-contains
   :nodefun-contents
   :nodefun-css
   :nodefun-data
   :nodefun-deepest
   :nodefun-each
   :nodefun-empty
   :nodefun-eq
   :nodefun-even
   :nodefun-filter
   :nodefun-find
   :nodefun-first
   :nodefun-gt
   :nodefun-has
   :nodefun-has-class
   :nodefun-hide
   :nodefun-html
   :nodefun-html-file
   :nodefun-index
   :nodefun-initialize
   :nodefun-insert-after
   :nodefun-insert-before
   :nodefun-is
   :nodefun-is-empty
   :nodefun-last
   :nodefun-length
   :nodefun-lt
   :nodefun-map
   :nodefun-next
   :nodefun-next-all
   :nodefun-next-until
   :nodefun-node
   :nodefun-not
   :nodefun-not-empty
   :nodefun-odd
   :nodefun-parent
   :nodefun-parents
   :nodefun-parents-until
   :nodefun-prepend
   :nodefun-prepend-to
   :nodefun-prev
   :nodefun-prev-all
   :nodefun-prev-until
   :nodefun-remove
   :nodefun-remove-attr
   :nodefun-remove-class
   :nodefun-remove-data
   :nodefun-replace-all
   :nodefun-replace-with
   :nodefun-show
   :nodefun-siblings
   :nodefun-size
   :nodefun-slice
   :nodefun-text
   :nodefun-toggle-class
   :nodefun-unwrap
   :nodefun-val
   :nodefun-wrap
   :nodefun-wrap-all
   :nodefun-wrap-inner
   :nodefun-write-to-file
   :nodefun-serialize))
