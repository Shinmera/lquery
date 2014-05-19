#|
  This file is a part of lQuery
  (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :lquery)

(defmacro define-node-function (name (node-name &rest arguments) &body body)
  "Defines a new node function. This is the main mechanism by which node manipulations are defined.
All node functions are automatically created in the lquery-funcs package.

NAME      --- A symbol naming the node function. Automatically interned in the LQUERY-FUNCS package.
NODE-NAME --- Symbol bound to the current node.
ARGUMENTS --- A lambda-list specifying the arguments for the function.
BODY      ::= form*"
  (let ((docstring (car body)))
    (if (stringp docstring)
        (setf body (cdr body))
        (setf docstring (format NIL "lQuery node function ~a" name)))
    (let ((funsymb (gensym "FUN"))
          (i (gensym "I")))
      `(defun ,(intern (symbol-name name) :lquery-funcs) (,node-name ,@arguments)
         ,docstring
         (flet ((,funsymb (,node-name) ,@body))
           (if (arrayp ,node-name)
               (loop for ,i from 0 below (length ,node-name)
                     do (setf (aref ,node-name ,i)
                              (,funsymb (aref ,node-name ,i)))
                     finally (return ,node-name))
               (,funsymb ,node-name)))
         ,node-name))))

(defmacro define-node-list-function (name (vector-name &rest arguments) &body body)
  "Defines a new function that operates on the current node array instead of individual elements.
All node list functions are automatically created in the lquery-funcs package.

NAME        --- A symbol naming the node function. Automatically interned in the LQUERY-FUNCS package.
VECTOR-NAME --- Symbol bound to the node vector.
ARGUMENTS   --- A lambda-list specifying the arguments for the function.
BODY        ::= form*"
  (let ((docstring (car body)))
    (if (stringp docstring)
        (setf body (cdr body))
        (setf docstring (format NIL "lQuery node list function ~a" name)))
    `(defun ,(intern (symbol-name name) :lquery-funcs) (,vector-name ,@arguments)
       ,docstring
       (let ((,vector-name (ensure-proper-vector ,vector-name)))
         ,@body))))

(defmacro $ (&body actions)
#.(format NIL "Performs lQuery operations on the current document.

Each argument is executed in sequence. The arguments are evaluated according to the defined argument-handlers. ~
By default, the following cases are handled: 
  * STRING    Translates to a CLSS:QUERY on the current elements.
  * LIST      What lists translate to is determined by their first element:
    * EVAL      The second value of the list is evaluated at compile-time and the result ~
                is inlined as if it had been there as a standard argument to $.
    * INLINE    The second value of the list is put in place and its run-time evaluation result ~
                will be handled as if by symbol. See the symbol-handlers for more information.
    * NODEFUN   If the symbol name is a node function, it is translated into a nodefun-call ~
                with the list of nodes as first argument and the other elements in the call-list ~
                as extra arguments.
    * T         Otherwise this list will be transformed into a regular function call with the ~
                current list of nodes as first argument and the other elements in the call-list ~
                as extra arguments.
  * FUNCTION  Translates to a function call with the list of nodes as argument.
  * SYMBOL    Delegates to the symbol handlers.

Symbols/Variables are handled at runtime (where they have a value) according to the defined ~
symbol-handlers. By default, the following cases are handled at run time:
  * STRING    Performs a CLSS:QUERY on the current elements.
  * DOM:NODE  Replaces the current set of nodes with just this node.  
  * FUNCTION  Calls the given function with the current set of nodes as argument.
  * LIST      Lists are transformed into a proper vector.
  * ARRAY     Arrays are transformed into a proper vector.
  * VECTOR    Vectors that are not adjustable are transformed into a proper vector.
  * T         Any other value simply replaces the current list of nodes.")
  (%$ (reverse actions)))

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
    (determine-list (find-symbol (string (car list)) :lquery) list nodes)))

(define-argument-handler symbol (symbol nodes)
  `(determine-value ,symbol ,nodes))

(define-argument-handler string (string nodes)
  `(clss:select ,string ,nodes))

(defgeneric determine-list (car list nodes)
  (:documentation "Determines what to do with a list."))

(defmethod determine-list (car list nodes)
  (multiple-value-bind (nodefun status) (find-symbol (symbol-name (car list)) :lquery-funcs)
    (if (and nodefun (eq status :EXTERNAL))
        `(,nodefun ,nodes ,@(cdr list))
        `(,(car list) ,nodes ,@(cdr list)))))

(defmacro define-list-handler (car-symbol (list-name operator-name) &body body)
  (assert (symbolp car-symbol))
  (setf car-symbol (or (find-symbol (string car-symbol) :lquery)
                       (intern (string car-symbol) :lquery)))
  `(defmethod determine-list ((,(gensym) (EQL ',car-symbol)) ,list-name ,operator-name)
     ,@body))

(define-list-handler function (list nodes)
  `(,(second list) ,nodes))

(define-list-handler eval (list nodes)
  `($ (inline ,nodes) ,(eval (second list))))

(define-list-handler inline (list nodes)
  `(determine-value ,(second list) ,nodes))

(define-list-handler combine (list nodes)
  (let ((node (gensym "NODE")))
    `(replace-vector ,nodes #'(lambda (,node) (list ,@(loop for call in (cdr list)
                                                            collect (determine-argument call node)))))))

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
