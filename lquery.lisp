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

(in-package :lquery)

;;
;; Should be elsewhere
;;

(defun mkstr (&rest args)
  "Concatenates args by printing into string."
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  "Interns the mkstr output/returns as symbol."
  (values (intern (apply #'mkstr args))))

(defun trim (string &optional (chars '(#\Space #\Tab #\Newline)))
  (string-trim chars string))

;;
;; Utility functions
;;
(defvar *lquery-master-document* NIL)

(defun assure-attribute (symbol-or-string)
  (trim
   (string-downcase
    (if (symbolp symbol-or-string)
        (symbol-name symbol-or-string)
        symbol-or-string))))

(defun build-elements (html &optional (document *lquery-master-document*))
  (let ((buildnode:*document* document))
    (coerce (dom:child-nodes (buildnode:inner-html html)) 'list)))

(defgeneric nodes-or-select (object &optional root)
  (:documentation "Return the object as a node list or use it to form a select query."))

(defmethod nodes-or-select ((selector-or-nodes string) &optional (root *lquery-master-document*))
  (css:query selector-or-nodes root))

(defmethod nodes-or-select ((selector-or-nodes list) &optional (root *lquery-master-document*))
  selector-or-nodes)

(defmethod nodes-or-select ((selector-or-nodes dom:node) &optional (root *lquery-master-document*))
  (list selector-or-nodes))


(defgeneric nodes-or-build (object &optional root)
  (:documentation "Clone the object as a node list or use it to build a new HTML node."))

(defmethod nodes-or-build ((html-or-nodes string) &optional (root *lquery-master-document*))
  (build-elements html-or-nodes root))

(defmethod nodes-or-build ((html-or-nodes list) &optional (root *lquery-master-document*))
  (loop for node in html-or-nodes collect (dom:clone-node node T)))

(defmethod nodes-or-build ((html-or-nodes dom:node) &optional (root *lquery-master-document*))
  (list (dom:clone-node html-or-nodes T)))


(defgeneric funcs-or-select (object)
  (:documentation "Return the object as a function or use it to construct a node-matches? function."))

(defmethod funcs-or-select ((selector-or-function string))
  (lambda (node) (funcall #'css:node-matches? node selector-or-function)))

(defmethod funcs-or-select ((selector-or-function function))
  selector-or-function)

(defgeneric list-or-selector-func (object)
  (:documentation "Build a function matching the selector or checking the equality/inclusion of the object."))

(defmethod list-or-selector-func ((selector string))
  (lambda (node) (css:node-matches? node selector)))

(defmethod list-or-selector-func ((nodes list))
  (lambda (node) (find node nodes)))

(defmethod list-or-selector-func ((checknode dom:node))
  (lambda (node) (eql node checknode)))

(defun get-css-styles (node)
  (let ((statements (split-sequence:split-sequence #\; (dom:get-attribute node "style")))
        (css-styles (make-hash-table)))
    (loop for statement in statements
         unless (= (length statement) 0)
         do (let ((keyval (split-sequence:split-sequence #\: statement)))
              (setf (gethash (symb (assure-attribute (first keyval))) css-styles) (second keyval))))
    css-styles))

(defun set-css-styles (node css-styles)
  (dom:set-attribute node "style"
                     (with-output-to-string (s)
                       (loop for key being the hash-keys of css-styles
                            for val being the hash-values of css-styles
                            unless (= (length val) 0)
                            do (format s "~a:~a;" (assure-attribute key) val)))))

;;
;; Base functions
;;
(defun load-page (file &optional (dtd NIL) (builder (cxml-dom:make-dom-builder)))
  "Load the given file into a HTML DOM. If dtd is set to non-NIL, the document is checked against the given DTD specification. Alternatively a cxml-dom builder can be specified as well."
  (with-open-file (html file :element-type '(unsigned-byte 8))
     (if dtd
         (cxml:parse html builder)
         (flet ((resolver (pubid sysid)
                  (declare (ignore pubid sysid))
                  (flexi-streams:make-in-memory-input-stream nil)))
           (cxml:parse html builder :entity-resolver #'resolver)))))

(defun initialize (document)
  "Sets the *lquery-master-document* variable to the provided document."
  (setf *lquery-master-document* document)
  document)

(defun parse-html (html)
  "Build the given string into DOM objects related to the master document."
  (build-elements html))

(defmacro defnodelistfun (function-name arguments &optional docstring &rest body)
  "Define a node function that operates on a list of nodes. The first argument in the arguments list must be the variable symbol for the provided list of nodes."
  (assert (first arguments) (arguments) "Arguments must at least contain node list variable name.")
  (let* ((list-name (first arguments))
         (arguments (cdr arguments))
         (function-name (symb 'nodefun- function-name))
         (generic-args (loop for arg in arguments
                          collect (if (listp arg) (first arg) arg))))
    (assert (symbolp list-name) (list-name) "Node list variable name (~S) must be a symbol." list-name)
    (if (not (stringp docstring)) (setf body (append body (list docstring))
                                        docstring ""))
    `(progn
       (defgeneric ,function-name (nodelist ,@generic-args)
         (:documentation ,docstring))
       (defmethod ,function-name ((,list-name list) ,@arguments)
         ,@body)
       (defmethod ,function-name ((,list-name dom:node) ,@arguments)
         (setf ,list-name (list ,list-name))
         ,@body)
       (symbol-function ',function-name))))

(defmacro defnodefun (function-name arguments &optional docstring &rest body)
  "Define a node function that operates on single nodes. The first argument in the arguments list must be the variable symbol for the provided list of nodes."
  (assert (first arguments) (arguments) "Arguments must at least contain node variable name.")
  (let* ((node-name (first arguments))
         (working-nodes (gensym "NDFUN-WRK-NODES"))
         (arguments (append (list working-nodes) (cdr arguments))))
    (assert (symbolp node-name) (node-name) "Node variable name (~S) must be a symbol." node-name)
    `(defnodelistfun ,function-name ,arguments ,docstring
                     (alexandria:flatten 
                      (loop for ,node-name in ,working-nodes collect
                           (progn ,@body))))))

(defmacro $ (&rest actions)
  "The main lQuery function to select and manipulate the DOM. The base syntax is as follows:
   ($ \"selector\" (function args*)*)
   
   Strings will evaluate to selectors and always act on the current set of elements.
   Function pointers will be called with the current set of elements passed to the function.
   A single element or a list replaces the current set of elements.
   
   An inline list represents a function call, although nodefun- and lquery functions take precedence.
   ($ (serialize)) evaluates (basically) to (nodefun-serialize working-nodes). However, if the named
   function is not a nodefun, packages are searched for a matching symbol in the following order: 
   lquery, *PACKAGE*, cl. If the function cannot be found in any of those, it is silently dropped."
  `(let ((working-nodes (list *lquery-master-document*)))
     (progn
       ,@(loop for action in actions collect
              `(setf working-nodes
                     ;Apparently function pointers are lists during macro expansion time so we need to catch that case.
                     ,(if (and (listp action) (not (eq (first action) 'FUNCTION)))
                          (let ((first (car action)))
                            (cond ((dom:node-p first) action)
                                  ((find-symbol (mkstr 'nodefun- first) :lquery)
                                   `(,(find-symbol (mkstr 'nodefun- first) :lquery) 
                                      working-nodes 
                                      ,@(cdr action)))
                                  (T (loop with name = (mkstr first) 
                                        for package in (list :lquery *PACKAGE* :cl)
                                        for symbol = (find-symbol name package)
                                        if symbol
                                        do (return `(,symbol ,@(cdr action)))))))
                          `($-helper ,action :working-nodes working-nodes)))))
     
     working-nodes))

(defgeneric $-helper (action &key &allow-other-keys)
  (:documentation "Helper function to determine what to do with a given variable at execution of the $ statement."))
(defmethod $-helper (action &key)
  action)
(defmethod $-helper ((action function) &key working-nodes)
  (funcall action working-nodes))
(defmethod $-helper ((action string) &key working-nodes)
  (css:query action working-nodes))
(defmethod $-helper ((action dom:node) &key)
  (list action))
