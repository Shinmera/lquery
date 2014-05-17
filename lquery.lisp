#|
  This file is a part of lQuery
  (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :lquery)

(defvar *lquery-master-document* NIL)

(defun make-proper-vector (&key (size 0) initial-element initial-contents (fill-pointer T))
  (cond
    (initial-element  (make-array size :initial-element initial-element :adjustable T :fill-pointer fill-pointer))
    (initial-contents (make-array size :initial-contents initial-contents :adjustable T :fill-pointer fill-pointer))
    (T                (make-array size :adjustable T :fill-pointer fill-pointer))))

(defgeneric copy-proper-vector (sequence &key transform)
  (:method ((vector sequence) &key (transform #'identity))
    (loop with result = (make-proper-vector :size (length vector) :fill-pointer T)
          for i from 0 below (length vector)
          do (setf (aref result i)
                   (funcall transform (aref vector i)))
          finally (return result)))
  (:method ((list list) &key (transform #'identity))
    (loop with length = (length list)
          with result = (make-proper-vector :size length :fill-pointer T)
          for i from 0 below length
          for item in list
          do (setf (aref result i)
                   (funcall transform item))
          finally (return result))))

(defun load-page (file-or-string)
  "Load the given file or string into a HTML DOM."
  (plump:parse file-or-string))

(defun initialize (document)
  "Sets the *lquery-master-document* variable to the provided document."
  (setf *lquery-master-document* document)
  document)

(defun parse-html (html)
  "Build the given string into DOM objects related to the master document."
  (lquery-funcs::build-elements html))

(defmacro define-node-function (name (node-name &rest arguments) &optional docstring &body body)
  "Defines a new node function. This is the main mechanism by which node manipulations are defined.
All node functions are automatically created in the lquery-funcs package."
  (unless (stringp docstring)
    (setf body (append (list docstring) body))
    (setf docstring (format NIL "lQuery node function ~a" name)))
  (let ((funsymb (gensym "FUN"))
        (i (gensym "I")))
    
    `(defun ,(intern (format NIL "NODEFUN-~a" name) :lquery-funcs) (,node-name ,@arguments)
       ,docstring
       (flet ((,funsymb (,node-name) ,@body))
         (if (arrayp ,node-name)
             (loop for ,i from 0 below (length ,node-name)
                   do (setf (aref ,node-name ,i)
                            (,funsymb (aref ,node-name ,i)))
                   finally (return ,node-name))
             (,funsymb ,node-name)))
       ,node-name)))

(defmacro define-node-list-function (name (list-name &rest arguments) &optional docstring &body body)
  "Defines a new function that operates on the current node array instead of individual elements.
All node list functions are automatically created in the lquery-funcs package."
  (unless (stringp docstring)
    (setf body (append (list docstring) body))
    (setf docstring (format NIL "lQuery node list function ~a" name)))
  (let ((declarations (loop for expr = (first body)
                         while (and expr (consp expr) (eql (first expr) 'DECLARE))
                         do (setf body (cdr body))
                         collect expr)))
    `(defun ,(intern (format NIL "NODEFUN-~a" name) :lquery-funcs) (,list-name ,@arguments)
       ,docstring
       ,@declarations
       (unless (arrayp ,list-name) (setf ,list-name (make-proper-vector :size 1 :initial-element ,list-name :fill-pointer T)))
       ,@body)))

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
  * T         Any other value simply replaces the current list of nodes.

Additional argument and symbol handlers can be defined with define-argument-handler and ~
define-symbol-handler, respectively.")
  (%$ (reverse actions)))

(defun %$ (actions)
  (if (null actions)
      `(make-proper-vector :size 1 :initial-element *lquery-master-document* :fill-pointer T)
      (let ((action (car actions))
            (rest (cdr actions)))
        (determine-argument action (%$ rest)))))

(defgeneric determine-argument (arg nodes)
  (:documentation "Determines what to do with a given argument at compile-time (static type)."))

(defmacro define-argument-handler (type (argname nodesname) &body body)
  "Defines a new argument handler that decides what to do with a certain type of argument at compile-time (static type)."
  `(defmethod determine-argument ((,argname ,type) ,nodesname)
     ,@body))

(define-argument-handler list (list nodes)
  (when list
    (let ((function (car list)))
      (cond 
        ((eq function 'FUNCTION) `(,(cadr list) ,nodes))
        ((eq function 'EVAL) `($ (inline ,nodes) ,(eval (second list))))
        ((eq function 'INLINE) `(determine-symbol ,(second list) ,nodes))
        (T (multiple-value-bind (nodefun status) (find-symbol (format NIL "NODEFUN-~a" function) :lquery-funcs)
             (if (and nodefun (eq status :EXTERNAL))
                 (append `(,nodefun ,nodes) (cdr list))
                 (append `(,function ,nodes) (cdr list)))))))))

(define-argument-handler symbol (symbol nodes)
  `(determine-symbol ,symbol ,nodes))

(define-argument-handler string (string nodes)
  `(clss:select ,string ,nodes))

(defgeneric determine-symbol (symbol nodes)
  (:documentation "Determines what to do with a given symbol at run-time (variable type)."))

(defmacro define-symbol-handler (type (symbolname nodesname) &body body)
  "Defines a new symbol handler that decides what to do with a certain type of symbol at run-time (variable type)."
  `(defmethod determine-symbol ((,symbolname ,type) ,nodesname)
     ,@body))

(define-symbol-handler T (variable nodes)
  (declare (ignorable nodes))
  variable)

(define-symbol-handler string (selector nodes)
  (clss:select selector nodes))

(define-symbol-handler plump:node (node nodes)
  (declare (ignorable nodes))
  (make-proper-vector :size 1 :initial-element node :fill-pointer T))

(define-symbol-handler function (function nodes)
  (funcall function nodes))
