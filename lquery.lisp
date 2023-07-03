(in-package #:org.shirakumo.lquery)

(defmacro deflqfun (name args &rest forms)
  (assert (symbolp name))
  `(defun ,(intern (symbol-name name) :lquery-funcs) ,args ,@forms))

(defmacro define-lquery-function (name (node-name &rest arguments) &body body)
  "Defines a new node function. This is the main mechanism by which node manipulations are defined.
All lquery functions are automatically created in the lquery-funcs package.

NAME      --- A symbol naming the lquery function. Automatically interned in the LQUERY-FUNCS package.
NODE-NAME --- Symbol bound to the current node.
ARGUMENTS --- A lambda-list specifying the arguments for the function.
BODY      ::= form*"
  (form-fiddle:with-destructured-lambda-form (:docstring doc :declarations decls :forms forms)
                                             `(lambda () ,@body)
    (let ((funsymb (gensym "FUN"))
          (i (gensym "I")))
      `(deflqfun ,name (,node-name ,@arguments)
         ,@(when doc (list doc))
         ,@decls
         (flet ((,funsymb (,node-name) ,@forms))
           (if (arrayp ,node-name)
               (loop for ,i from 0 below (length ,node-name)
                     do (setf (aref ,node-name ,i)
                              (,funsymb (aref ,node-name ,i)))
                     finally (return ,node-name))
               (,funsymb ,node-name)))))))

(defmacro define-lquery-list-function (name (vector-name &rest arguments) &body body)
  "Defines a new function that operates on the current node array instead of individual elements.
All lquery functions are automatically created in the lquery-funcs package.

NAME        --- A symbol naming the lquery function. Automatically interned in the LQUERY-FUNCS package.
VECTOR-NAME --- Symbol bound to the node vector.
ARGUMENTS   --- A lambda-list specifying the arguments for the function.
BODY        ::= form*"
  (form-fiddle:with-destructured-lambda-form (:docstring doc :declarations decls :forms forms)
                                             `(lambda () ,@body)
    `(deflqfun ,name (,vector-name ,@arguments)
       ,@(when doc (list doc))
       ,@decls
       (let ((,vector-name (ensure-proper-vector ,vector-name)))
         ,@forms))))

(defmacro define-lquery-subroutine (name (&rest arguments) &body body)
  "Defines a shorthand function. The body is a set of lQuery instructions as you'd use in $.

NAME      --- A symbol naming the subroutine. Automatically interned in the LQUERY-FUNCS package.
ARGUMENTS --- A lambda-list specifying the arguments for the function.
BODY      ::= lquery-form*"
  (let ((prev (gensym "PREV")))
    (labels ((%$ (forms)
               (if (null forms)
                   prev
                   (determine-argument (car forms) (%$ (cdr forms))))))
      `(deflqfun ,name (,prev ,@arguments)
         ,(%$ (reverse body))))))

(defmacro define-lquery-macro (name (previous-form &rest arguments) &body body)
  "Define a new lquery local macro.
All lquery macros are automatically created in the lquery-macros package.

NAME          --- A symbol naming the lquery macro. Automatically interned in the LQUERY-MACROS package.
PREVIOUS-FORM --- Symbol bound to the so far assembled form, the previous value so to speak.
ARGUMENTS     --- A lambda-list specifying the arguments for the macro (note that this must be a standard lambda-list).
BODY          ::= form*"
  (assert (symbolp name))
  (let ((docstring (car body)))
    (if (stringp docstring)
        (setf body (cdr body))
        (setf docstring (format NIL "lQuery node macro ~a" name)))
    `(defun ,(intern (symbol-name name) :lquery-macros) (,previous-form ,@arguments)
       ,docstring
       ,@body)))

(defmacro $ (&body actions)
#.(format NIL "Performs lQuery operations on the current document.

Each argument is executed in sequence. The arguments are evaluated according to the defined argument-handlers. ~
By default, the following cases are handled: 
  * STRING    Translates to a CLSS:QUERY on the current elements.
  * FUNCTION  Translates to a function call with the list of nodes as argument.
  * SYMBOL    Delegates to the value handlers.
  * LIST      Lists are transformed according to their first element, which must be a symbol. ~
              If the symbol's name corresponds to a function found in the LQUERY-MACROS package, ~
              The form is assembled according to that function. Otherwise if it corresponds to an ~
              LQUERY-FUNCS function, it is expanded into a call to that function. If the symbol ~
              cannot be found in either package, it is put back in place, but the call itself is ~
              assembled like so: (FUNCTION PREVIOUS-RESULT ARGUMENT*)

Values are handled at runtime according to the defined variable-handlers. ~
By default, the following cases are handled at run time:
  * STRING    Performs a CLSS:QUERY on the current elements.
  * DOM:NODE  Replaces the current set of nodes with just this node.  
  * FUNCTION  Calls the given function with the current set of nodes as argument.
  * LIST      Lists are transformed into a proper vector.
  * ARRAY     Arrays are transformed into a proper vector.
  * VECTOR    Vectors that are not adjustable are transformed into a proper vector.
  * T         Any other value simply replaces the current list of nodes.")
  (%$ (reverse actions)))

(defmacro $1 (&body actions)
  "This is the same as $, except it automatically uses NODE at the end and thus only returns the first result, if any."
  `($ ,@actions (node)))

(defun %$ (actions)
  (if (null actions)
      `(make-proper-vector :size 1 :initial-element *lquery-master-document* :fill-pointer T)
      (let ((action (car actions))
            (rest (cdr actions)))
        (determine-argument action (%$ rest)))))

(defgeneric determine-argument (arg nodes)
  (:documentation "Determines what to do with a given argument at compile-time (static type)."))

(defmacro define-argument-handler (type (argument-name operator-name) &body body)
  "Defines a new argument handler that decides what to do with a certain type of argument at compile-time.

TYPE          --- A type or EQL specifier.
ARGUMENT-NAME --- Symbol bound to the argument.
OPERATOR-NAME --- Symbol bound to the object being operated on.
BODY          ::= form*"
  `(defmethod determine-argument ((,argument-name ,type) ,operator-name)
     ,@body))

(define-argument-handler list (list nodes)
  (when list
    (etypecase (car list)
      (symbol
       (let ((nodemacro (find-symbol (symbol-name (car list)) :lquery-macros)))
         (if nodemacro
             (apply (symbol-function nodemacro) nodes (cdr list))
             (let ((nodefunc (find-symbol (symbol-name (car list)) :lquery-funcs)))
               (if nodefunc
                   `(,nodefunc ,nodes ,@(cdr list))
                   `(,(car list) ,nodes ,@(cdr list))))))))))

(define-argument-handler symbol (symbol nodes)
  `(determine-value ,symbol ,nodes))

(define-argument-handler string (string nodes)
  `(clss:select ,string ,nodes))

(defgeneric determine-value (symbol nodes)
  (:documentation "Determines what to do with a given symbol at run-time (variable type)."))

(defmacro define-value-handler (type (variable-name operator-name) &body body)
  "Defines a new symbol handler that decides what to do with a certain type of symbol at run-time (variable type).

TYPE          --- A type or EQL specifier.
VARIABLE-NAME --- Symbol bound to the argument.
OPERATOR-NAME --- Symbol bound to the object being operated on.
BODY          ::= form*"
  `(defmethod determine-value ((,variable-name ,type) ,operator-name)
     ,@body))

(define-value-handler T (variable nodes)
  (declare (ignore nodes))
  variable)

(define-value-handler list (list nodes)
  (declare (ignore nodes))
  (copy-proper-vector list))

(define-value-handler array (array nodes)
  (declare (ignore nodes))
  (copy-proper-vector array))

(define-value-handler vector (vector nodes)
  (declare (ignore nodes))
  (if (adjustable-array-p vector)
      vector
      (copy-proper-vector vector)))

(define-value-handler string (selector nodes)
  (clss:select selector nodes))

(define-value-handler plump:node (node nodes)
  (declare (ignore nodes))
  (make-proper-vector :size 1 :initial-element node :fill-pointer T))

(define-value-handler function (function nodes)
  (funcall function nodes))
