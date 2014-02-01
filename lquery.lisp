#|
  This file is a part of lQuery
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :lquery)

(defvar *lquery-master-document* NIL)

(defun load-file-quickly (pathname)
  (with-open-file (stream pathname)
    (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
      (setf (fill-pointer seq) (read-sequence seq stream))
      seq)))

(defun load-page (file-or-string &key (type :XML) (builder (cxml-dom:make-dom-builder)))
  "Load the given file or string into a HTML DOM.
If type is :HTML, the file is treated as a full HTML document.
If type is NIL or :XML, the file is treated as an XML document, which does not need to contain the HTML root nodes.
Alternatively, the type can be a DTD specification that will be supplied to cxml:parse 's dtd keyword."
  (flet ((parse (html)
           (case type
             (:HTML
              (chtml:parse html builder))
             ((NIL :XML)
              (flet ((resolver (pubid sysid)
                       (declare (ignore pubid sysid))
                       (flexi-streams:make-in-memory-input-stream nil)))
                (cxml:parse html builder :entity-resolver #'resolver)))
             (T 
              (cxml:parse html builder :dtd type)))))
    (etypecase file-or-string
      (pathname (with-open-file (html file-or-string :element-type '(unsigned-byte 8))
                  (parse html)))
      (string (parse file-or-string)))))

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
  (let ((argslist (cons node-name
                        (loop for arg in arguments
                           unless (find arg '(&optional &key &allow-other-keys &rest))
                           collect (if (listp arg) (first arg) arg))))
        (funsymb (gensym "FUN")))
        
    `(defun ,(intern (format NIL "NODEFUN-~a" name) :lquery-funcs) (,node-name ,@arguments)
       ,docstring
       (flet ((,funsymb ,argslist ,@body))
         (if (listp ,node-name)
             (alexandria:flatten 
              (mapcar #'(lambda (,node-name) (,funsymb ,@argslist)) ,node-name))
             (,funsymb ,@argslist))))))

(defmacro define-node-list-function (name (list-name &rest arguments) &optional docstring &body body)
  "Defines a new function that operates on the current node list instead of individual elements.
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
       (unless (listp ,list-name) (setf ,list-name (list ,list-name)))
       ,@body)))

(defmacro $ (&body actions)
#.(format NIL "Performs lQuery operations on the current document.

Each argument is executed in sequence. The arguments are evaluated according to the defined argument-handlers. ~
By default, the following cases are handled: 
  * STRING    Translates to a CSS-select on the current elements.
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
  * STRING    Performs a CSS-select on the current elements.
  * DOM:NODE  Replaces the current set of nodes with just this node.  
  * FUNCTION  Calls the given function with the current set of nodes as argument.
  * T         Any other value simply replaces the current list of nodes.

Additional argument and symbol handlers can be defined with define-argument-handler and ~
define-symbol-handler, respectively.")
  (%$ (reverse actions)))

(defun %$ (actions)
  (if (zerop (length actions))
      `(list *lquery-master-document*)
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
  `(css:query ,string ,nodes))

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
  (css:query selector nodes))

(define-symbol-handler dom:node (node nodes)
  (declare (ignorable nodes))
  (list node))

(define-symbol-handler function (function nodes)
  (funcall function nodes))
