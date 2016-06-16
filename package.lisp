#|
  This file is a part of lQuery
  (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:lquery
  (:nicknames #:org.shirakumo.lquery)
  (:use #:cl #:array-utils)
  (:export
   #:*lquery-master-document*
   #:make-proper-vector
   #:copy-proper-vector
   #:ensure-proper-vector
   #:parse-html
   #:initialize
   #:load-page
   
   #:$
   #:$1
   #:inline
   #:eval

   #:define-lquery-function
   #:define-lquery-list-function
   #:define-lquery-subroutine
   #:define-lquery-macro
   #:define-value-handler
   #:define-argument-handler))

(defpackage #:lquery-funcs
  (:nicknames #:org.shirakumo.lquery.funcs)
  (:use)
  (:export
   #:add
   #:add-class
   #:after
   #:ancestor
   #:append
   #:append-to
   #:attr
   #:before
   #:children
   #:child-index
   #:clone
   #:closest
   #:contains
   #:contents
   #:css
   #:data
   #:deepest
   #:detach
   #:each
   #:empty
   #:empty-p
   #:eq
   #:even
   #:filter
   #:find
   #:first
   #:gt
   #:has
   #:has-class
   #:hide
   #:html
   #:html-file
   #:index
   #:initialize
   #:insert-after
   #:insert-before
   #:is
   #:last
   #:length
   #:lt
   #:map
   #:map-apply
   #:next
   #:next-all
   #:next-until
   #:node
   #:not
   #:not-empty
   #:odd
   #:parent
   #:parents
   #:parents-until
   #:prepend
   #:prepend-to
   #:prev
   #:prev-all
   #:prev-until
   #:remove
   #:remove-attr
   #:remove-class
   #:remove-data
   #:replace-all
   #:replace-with
   #:root
   #:show
   #:siblings
   #:size
   #:slice
   #:splice
   #:text
   #:toggle-class
   #:unwrap
   #:val
   #:wrap
   #:wrap-all
   #:wrap-inner
   #:write-to-file
   #:serialize))

(defpackage #:lquery-macros
  (:nicknames #:org.shirakumo.lquery.macros)
  (:use)
  (:export
   #:inline
   #:eval
   #:combine
   #:initialize
   #:function))
