#|
 This file is a part of lQuery
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :lquery)

(define-lquery-macro function (nodes name)
  `(,name ,nodes))

(define-lquery-macro eval (nodes form)
  `($ (inline ,nodes) ,(eval form)))

(define-lquery-macro inline (nodes form)
  `(determine-value ,form ,nodes))

(define-lquery-macro combine (nodes &rest calls)
  (let ((node (gensym "NODE")))
    `(lquery-funcs:map ,nodes #'(lambda (,node) (list ,@(loop for call in calls
                                                              collect (determine-argument call node)))))))

(define-lquery-macro initialize (nodes &rest init-calls)
  (declare (ignore nodes))
  `(lquery-funcs:initialize NIL ,@init-calls))
