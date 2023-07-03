(in-package #:org.shirakumo.lquery)

(define-lquery-macro function (nodes name)
  "Macro to allow #'foo to be used in lquery chains."
  `(,name ,nodes))

(define-lquery-macro eval (nodes form)
  "Evaluates the form at compile-time and puts its resulting value in place."
  `($ (inline ,nodes) ,(eval form)))

(define-lquery-macro inline (nodes form)
  "Treats the form as if the evaluated value was put literally in place.

See DETERMINE-VALUE."
  `(determine-value ,form ,nodes))

(define-lquery-macro combine (nodes &rest calls)
  "COMBINES multiple lquery function calls into one by gathering them into a list for each element.

 ($ (combine (text) (attr :a))) would be  equivalent to 
 ($ (map #'(lambda (node) (list (lquery-funcs:text node) (lquery-funcs:attr node :a)))))
This construct is especially useful in combination with MAP-APPLY."
  (let ((node (gensym "NODE")))
    `(lquery-funcs:map ,nodes #'(lambda (,node) (list ,@(loop for call in calls
                                                              collect (determine-argument call node)))))))

(define-lquery-macro initialize (nodes &rest init-calls)
  "See lquery function INITIALIZE.

This is merely a performance macro to avoid the unnecessary default allocation of a vector."
  (declare (ignore nodes))
  `(lquery-funcs:initialize NIL ,@init-calls))

(define-lquery-macro $ (nodes &rest forms)
  "Shorthand to allow dropping the package prefix when nesting $/$1.

See $"
  `(lquery:$ ,nodes ,@forms))

(define-lquery-macro $1 (nodes &rest forms)
  "Shorthand to allow dropping the package prefix when nesting $/$1.

See $1"
  `(lquery:$1 ,nodes ,@forms))
