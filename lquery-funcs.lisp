#|
  This file is a part of lQuery
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :lquery-funcs)

(import '(lquery::make-proper-vector
          lquery::copy-proper-vector))

(defun trim (string &optional (chars '(#\Space #\Tab #\Newline)))
  (string-trim chars string))

(defun mkstr (&rest args)
  "Concatenates args by printing into string."
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  "Interns the mkstr output/returns as symbol."
  (let ((name (apply #'mkstr args)))
    (values (or (find-symbol name)
                (intern name)))))

(defun assure-attribute (symbol-or-string)
  (trim
   (string-downcase
    (etypecase symbol-or-string
      (string symbol-or-string)
      (symbol (symbol-name symbol-or-string))))))

(defun build-elements (html)
  (plump:children (plump:parse html)))

(defgeneric nodes-or-select (object &optional root)
  (:documentation "Return the object as a node list or use it to form a select query.")
  (:method ((string string) &optional (root *lquery-master-document*))
    (clss:select string root))
  (:method ((vector vector) &optional (root *lquery-master-document*))
    (declare (ignore root))
    vector)
  (:method ((list list) &optional (root *lquery-master-document*))
    (declare (ignore root))
    (copy-proper-vector list))
  (:method ((node plump:node) &optional (root *lquery-master-document*))
    (declare (ignore root))
    (make-proper-vector :size 1 :initial-element node)))

(defgeneric nodes-or-build (object)
  (:documentation "Clone the object as a node list or use it to build a new HTML node.")
  (:method ((html string))
    (build-elements html))
  (:method ((vector vector))
    (copy-proper-vector vector :transform #'plump:clone-node))
  (:method ((list list))
    (copy-proper-vector list :transform #'plump:clone-node))
  (:method ((node plump:node))
    (make-proper-vector :size 1 :initial-element (plump:clone-node node))))

(defgeneric funcs-or-select (object)
  (:documentation "Return the object as a function or use it to construct a node-matches? function.")
  (:method ((selector string))
    (let ((selector (clss:parse-selector selector)))
      #'(lambda (node)
          (clss:node-matches-p selector node))))
  (:method ((function function))
    function))

(defgeneric list-or-selector-func (object)
  (:documentation "Build a function matching the selector or checking the equality/inclusion of the object.")
  (:method ((selector string))
    (let ((selector (clss:parse-selector selector)))
      #'(lambda (node)
          (clss:node-matches-p selector node))))
  (:method ((nodes list))
    #'(lambda (node) (find node nodes)))
  (:method ((checknode plump:node))
    #'(lambda (node) (eql node checknode))))

(defun get-css-styles (node)
  (loop with table = (make-hash-table :test 'equalp)
        for statement in (split-sequence:split-sequence #\; (plump:attribute node "style"))
        unless (= (length statement) 0)
          do (let ((keyval (split-sequence:split-sequence #\: statement)))
               (setf (gethash (assure-attribute (first keyval)) table) (second keyval)))
        finally (return table)))

(defun set-css-styles (node css-styles)
  (setf (plump:attribute node "style")
        (with-output-to-string (s)
          (loop for key being the hash-keys of css-styles
                for val being the hash-values of css-styles
                unless (= (length val) 0)
                  do (format s "~a: ~a;" (assure-attribute key) val)))))


(define-node-list-function add (working-nodes selector-or-nodes)
  "Add elements to the set of matched elements."
  (plump::vector-append working-nodes (nodes-or-select selector-or-nodes)))

(define-node-function add-class (node &rest classes)
  "Adds the specified class(es) to the set of matched elements."
  (setf (plump:attribute node "class")
        (with-output-to-string (stream (plump:attribute node "class"))
          (format stream "~{ ~a~}" classes)))
  node)

(define-node-function after (node html-or-nodes)
  "Insert content (in html-string or node-list form) after each element."
  (plump::vector-append (plump:family node) (nodes-or-build html-or-nodes))
  node)

;;@todo
(define-node-list-function ancestor (working-nodes)
  "Find the common ancestor of all elements."
  (loop with parentlists = (loop for node in working-nodes 
                              collect (reverse (nodefun-parents node)))
     for i = 0 then (1+ i)
     for prevparents = NIL then parents
     for parents = (loop for list in parentlists for el = (nth i list) if el collect el)
     until (or (not (every #'eql (list (first parents)) parents)) 
               (not (= (length parents) (length parentlists))))
     finally (return (list (first prevparents)))))

(define-node-function append (node html-or-nodes)
  "Insert content (in html-string or node-list form) to the end of each element."
  (plump::vector-append (plump:children node) (nodes-or-build html-or-nodes)) 
  node)

(define-node-list-function append-to (working-nodes selector-or-nodes)
  "Insert every element to the end of the target(s)."
  (loop for target across (nodes-or-select selector-or-nodes)
        do (plump::vector-append (plump:children target) working-nodes))
  working-nodes)

(define-node-function attr (node &rest pairs)
  "Retrieve or set attributes on a node"
  (case (length pairs)
    (0 (error "Attribute arguments must be one or more attributes or one or more key-value pairs."))
    (1 (plump:attribute node (assure-attribute (first pairs))))
    (otherwise 
     (loop for (key val) on pairs by #'cddr
           do (setf (plump:attribute node key) val))
     node)))

(define-node-function before (node html-or-nodes)
  "Insert content (in html-string or node-list form) before each element."
  (plump::vector-append (plump:family node) (nodes-or-build html-or-nodes) (plump:child-position node))
  node)

(define-node-function children (node &optional selector)
  "Get the children of each element, optionally filtered by a selector."
  (if selector
      (clss:select selector node)
      (plump:child-elements node)))

(define-node-function child-index (node)
  "Returns the index of the element within its parent, also counting text nodes. See index() otherwise."
  (plump:child-position node))

(define-node-function clone (node)
  "Create a deep copy of the set of matched elements."
  (plump:clone-node node))

(define-node-function closest (node selector)
  "For each element in the set, get the first element that matches the selector by testing the element itself and traversing up through its ancestors in the DOM tree.
If no matching element can be found the root is entered instead."
  (when (stringp selector)
    (setf selector (clss:parse-selector selector)))
  (loop for node = (plump:parent node)
        until (or (plump:root-p node)
                  (clss:node-matches-p selector node)))
  node)

(define-node-list-function contains (nodes string)
  "Select all elements that contain the specified text."
  (loop with string = (trim string)
        with result = (make-proper-vector)
        for node across nodes
        do (when (string= string (trim (plump:text node)))
             (vector-push-extend node result))
        finally (return result)))

(define-node-list-function contents (nodes)
  "Get the children of each element, including text and comment nodes."
  (loop with result = (make-proper-vector)
        for node across nodes
        do (loop for child across (plump:children node)
                 do (vector-push-extend child result))
        finally (return result)))

(define-node-function css (node &rest pairs)
  "Retrieve or set css style attributes on a node."
  (let ((css-styles (get-css-styles node)))
    (case (length pairs)
      (0 (error "CSS attribute arugments must be one or more attributes or one or more key-value pairs."))
      (1 (gethash (assure-attribute (first pairs)) css-styles))
      (otherwise
       (set-css-styles node
                       (loop for (key val) on pairs by #'cddr
                             do (setf (gethash (assure-attribute key) css-styles) val)))
       node))))

(define-node-function data (node &rest pairs)
  "Retrieve or set data attributes on a node. This is a convenience method and uses attr in the back."
  (case (length pairs)
    (0 (error "Data attribute arguments must be one or more attributes or one or more key-value pairs."))
    (1 (plump:attribute node (concatenate 'string "data-" (assure-attribute (first pairs)))))
    (otherwise
     (loop for (key val) on pairs by #'cddr
           do (setf (plump:attribute node (concatenate 'string "data-" (assure-attribute (first pairs))))
                    val))
     node)))

(define-node-function deepest (node)
  "Returns the innermost (left-bound) child element."
  (labels ((r (node)
             (loop with elp = NIL
                   for child across (plump:children node)
                   when (plump:element-p child)
                     do (setf elp child)
                   until elp
                   finally (if elp
                               (r elp)
                               node))))
    (r node))) 

(define-node-list-function detach (nodes &optional selector)
  "Removes the node (optionally filtered by the selector) from the document. Alias for remove()"
  (nodefun-remove nodes selector))

(define-node-list-function each (nodes fun &key replace)
  "Execute the specified function on each element until NIL is returned or all elements have been processed. The original set of elements is returned if replace is NIL."
  (if replace
      (loop for i from 0 below (length nodes)
            for result = (funcall fun (aref nodes i))
            while result
            do (setf (aref nodes i) result)
            finally (setf (fill-pointer nodes) i))
      (loop for node across nodes
            while (funcall fun node)))
  nodes)

(define-node-function empty (node)
  "Remove all child nodes from the set of matched elements."
  (plump:clear node))

(define-node-list-function eq (working-nodes index)
  "Reduce the set of matched elements to the one at the specified index"
  (setf (aref working-nodes 0) (aref working-nodes index)
        (fill-pointer working-nodes) 1)
  working-nodes)

(define-node-list-function even (working-nodes)
  "Selects even elements, 1-indexed"
  (loop for i from 0
        while (< (1+ (* i 2)) (length working-nodes))
        do (setf (aref working-nodes i)
                 (aref working-nodes (1+ (* i 2))))
        finally (setf (fill-pointer working-nodes) i))
  working-nodes)

(define-node-list-function filter (nodes selector-or-function)
  "Reduce the set of matched elements to those that match the selector or pass the function's test."
  (loop with i = 0
        with fun = (funcs-or-select selector-or-function)
        for node across nodes
        do (when (funcall fun node)
             (setf (aref nodes i) node)
             (incf i))
        finally (setf (fill-pointer nodes) i))
  nodes)

(define-node-list-function find (nodes selector-or-function &key (test-self NIL))
  "Get the descendants of each element filtered by selector or function."
  (loop with result = (make-proper-vector)
        with func = (funcs-or-select selector-or-function)
        for node across nodes
        do (labels ((r (node)
                      (loop for child across (plump:children node)
                            do (r child)
                            when (funcall func child)
                              do (vector-push-extend child result))))
             (r node))
           (when (and test-self (funcall func node))
             (vector-push-extend node result))
        finally (return result)))

(define-node-list-function first (working-nodes)
  "Reduce the set of matched elements to the first in the set."
  (setf (fill-pointer working-nodes) 1))

(define-node-list-function gt (working-nodes index)
  "Select all elements at a greater than index(0) within the matched set."
  (loop for i from 0 below (- (length working-nodes) index)
        do (setf (aref working-nodes i)
                 (aref working-nodes (+ i index)))
        finally (setf (fill-pointer working-nodes) (- (length working-nodes) index)))
  working-nodes)

(define-node-list-function has (nodes selector-or-nodes)
  "Reduce the set of matched elements to those that have a descendant that matches the selector or element."
  (loop with i = 0
        with find-fun = (list-or-selector-func selector-or-nodes)
        for node across nodes
        do (unless (= 0 (length (nodefun-find node find-fun)))
             (setf (aref nodes i) node))
        finally (setf (fill-pointer nodes) i))
  nodes)

(define-node-list-function has-class (working-nodes class)
  "Determine whether any of the matched elements are assigned to the given class."
  (let ((class (assure-attribute class)))
    (loop for node across working-nodes
          if (find class
                   (split-sequence:split-sequence #\space (plump:attribute node "class")) 
                   :test #'string-equal)
            return T)))

(define-node-list-function hide (working-nodes )
  "Hide the matched elements (short for (css \"display\" \"none\"))."
  (nodefun-css working-nodes "display" "none"))

(define-node-function html (node &optional new-content)
  "Get the HTML contents of the elements or set the HTML contents of every matched element."
  (if new-content
      (progn
        (plump:clear node)
        (plump:parse new-content :root node)
        node)
      (plump:serialize node NIL)))

(define-node-list-function html-file (working-nodes pathname)
  "Read an HTML file and insert its contents into each element."
  (let ((document (plump:parse pathname)))
    (loop for node across working-nodes
          do (plump:clear node)
             (loop for child across (plump:children document)
                   do (plump:append-child node (plump:clone-node child))))
    working-nodes))

(define-node-function index (node)
  "Find the index of the node within its parent."
  (plump:element-position node))

(define-node-list-function initialize (working-nodes document)
  "Re-initializes lQuery with a new page."
  (make-proper-vector :size 1 :initial-element (initialize (load-page document))))

(define-node-list-function insert-after (working-nodes selector-or-nodes)
  "Insert every element after the target."
  (nodefun-after (nodes-or-select selector-or-nodes) working-nodes))

(define-node-list-function insert-before (working-nodes selector-or-nodes)
  "Insert every element before the target."
  (nodefun-before (nodes-or-select selector-or-nodes) working-nodes))

(define-node-list-function is (working-nodes selector-or-nodes)
  "Check the current elements against a selector or list of elements and return true if at least one of them matches."
  (let ((find-fun (list-or-selector-func selector-or-nodes)))
    (loop for node across working-nodes
          thereis (funcall find-fun node))))

(define-node-list-function is-empty (working-nodes)
  "Check if the node contains no children and/or only empty (whitespace) text nodes. If it is empty, T is returned, otherwise NIL."
  (not (nodefun-not-empty working-nodes)))

(define-node-list-function last (working-nodes)
  "Reduce the set of matched elements to the final one in the set."
  (when (< 0 (length working-nodes))
    (setf (aref working-nodes 0)
          (aref working-nodes (1- (length working-nodes)))
          (fill-pointer working-nodes) 1))
  working-nodes)

(define-node-list-function length (working-nodes)
  "Returns the number of elements in the list."
  (length working-nodes))

(define-node-list-function lt (working-nodes index)
  "Select all elements at an index less than the index within the matched set."
  (setf (fill-pointer working-nodes) index)
  working-nodes)

(define-node-list-function map (working-nodes function)
  "Pass each element through a function (which has to accept one argument, the node), returning the list of all results."
  (loop with i = 0
        for node across working-nodes
        do (when (funcall function node)
             (setf (aref working-nodes i) node))
        finally (setf (fill-pointer working-nodes) i))
  working-nodes)

(define-node-list-function next (nodes &optional selector)
  "Get the immediately following sibling of each element (if there is one). If a selector is provided, the sibling is only included if it matches."
  (when (stringp selector)
    (setf selector (clss:parse-selector selector)))
  (loop with i = 0
        for node across nodes
        for sibling = (plump:next-element node)
        do (when (and sibling (or (not selector) (clss:node-matches-p selector sibling)))
             (setf (aref nodes i) sibling)
             (incf i))
        finally (setf (fill-pointer nodes) i))
  nodes)

(define-node-list-function next-all (nodes &optional selector)
  "Get all following siblings of each element. If a selector is provided, the sibling is only included if it matches."
  (when (stringp selector)
    (setf selector (clss:parse-selector selector)))
  (loop with result = (make-proper-vector)
        for node across nodes
        do (loop for i from (1+ (plump:child-position node))
                   below (length (plump:family node))
                 do (let ((sibling (aref (plump:family node) i)))
                      (when (and (plump:element-p sibling)
                                 (clss:node-matches-p selector sibling))
                        (vector-push-extend sibling result))))
        finally (return result)))

(define-node-function next-until (node selector-or-nodes)
  "Get all following silings of each element up to (excluding) the element matched by the selector or node list."
  (let ((family (nodefun-next-all node))
        (find-fun (list-or-selector-func selector-or-nodes)))
    (loop for sibling in family
       until (funcall find-fun sibling)
       collect sibling)))

(define-node-list-function node (working-nodes &optional (n 0))
  "Return the specified node (default first) directly, without encompassing it into a list."
  (nth n working-nodes))

(define-node-list-function not (working-nodes selector-or-nodes)
  "Remove elements from the set of matched elements."
  (remove-if-not (list-or-selector-func selector-or-nodes) working-nodes))

(define-node-function not-empty (node)
  "Check if the node contains no children and/or only empty (whitespace) text nodes. If the node is effectively empty, NIL is returned. Otherwise a list of all non-empty children and text-nodes is returned."
  (loop for child across (dom:child-nodes node)
     unless (or (and (dom:text-node-p child)
                     (= 0 (length (trim (dom:data child)))))
                (dom:comment-p child))
     collect child))

(define-node-list-function odd (working-nodes)
  "Select all odd elements from the current set."
  (loop for node in working-nodes
       for i from 1 upto (length working-nodes)
       if (oddp i)
       collect node))

(define-node-function parent (node &optional selector)
  "Get the parent of each element, optionally filtered by a selector."
  (let ((parent (dom:parent-node node)))
    (if (or (not selector) (css:node-matches? parent selector))
        parent)))

(define-node-function parents (node &optional selector)
  "Get the ancestors of each element, optionally filtered by a selector. Closest parent first."
    (loop for parent = (dom:parent-node node) then (dom:parent-node parent)
       while (not (dom:document-p parent))
       if (or (not selector) (css:node-matches? parent selector))
       collect parent))

(define-node-function parents-until (node selector-or-nodes)
  "Get the ancestors of each element, up to (excluding) the element matched by the selector or node list. Closest parent first"
  (loop for parent = (dom:parent-node node) then (dom:parent-node parent)
     with find-fun = (list-or-selector-func selector-or-nodes)
     while (and (not (dom:document-p parent))
                (not (funcall find-fun parent)))
     collect parent))

(define-node-function prepend (node html-or-nodes)
  "Insert content, specified by the parameter, to the beginning of each element."
  (apply #'buildnode:insert-nodes node 0 (nodes-or-build html-or-nodes))
  node)

(define-node-list-function prepend-to (working-nodes selector-or-nodes)
  "Insert every element to the beginning of the target(s)."
  (loop for target in (nodes-or-select selector-or-nodes)
     do (nodefun-prepend target working-nodes))
  working-nodes)

(define-node-function prev (node &optional selector)
  "Get the immediately preceding sibling of each element (if there is one). If a selector is provided, the sibling is only included if it matches."
  (let ((family (nodefun-children (nodefun-parent node)))
        (index (nodefun-index node)))
    (if (> index 0)
        (let ((sibling (nth (1- index) family)))
          (if (or (not selector) (css:node-matches? sibling selector))
              sibling)))))

(define-node-function prev-all (node &optional selector)
  "Get all preceeding siblings of each element. If a selector is provided, the sibling is only included if it matches."
  (let ((family (nodefun-children (nodefun-parent node)))
        (index (nodefun-index node)))
    (if (> index 0)
        (let ((family (reverse (subseq family 0 index))))
          (if selector 
              (remove-if-not (lambda (node) (css:node-matches? node selector)) family)
              family)))))

(define-node-function prev-until (node selector-or-nodes)
  "Get all preceeding silings of each element down to (excluding) the element matched by the selector or node list."
  (let ((family (nodefun-prev-all node))
        (find-fun (list-or-selector-func selector-or-nodes)))
    (loop for sibling in family
       until (funcall find-fun sibling)
       collect sibling)))

(define-node-function remove (node &optional selector)
  "Remove the set of matched elements from the DOM."
  (when (or (not selector) (clss:node-matches-p selector node))
    (plump:remove-child node)))

(define-node-function remove-attr (node &rest attributes)
  "Remove attributes from each element."
  (loop for attr in attributes
     do (setf attr (assure-attribute attr))
     if (dom:has-attribute node attr)
       do (dom:remove-attribute node attr))
  node)

(define-node-function remove-class (node &rest classes)
  "Remove classes from each element."
  (loop for classlist = (split-sequence:split-sequence #\space (trim (dom:get-attribute node "class")))
     then (remove class classlist :test #'string-equal) 
     for class in classes
     do (setf class (assure-attribute class))
     finally (dom:set-attribute node "class" (format nil "~{~A~^ ~}" classlist)))
  node)

(define-node-list-function remove-data (working-nodes &rest data)
  "Remove data attributes from each element. This is a convenience method and uses remove-attr in the back."
  (apply #'nodefun-remove-attr 
         working-nodes 
         (mapcar (lambda (it) (concatenate 'string "data-" (assure-attribute it))) data)))

(define-node-list-function replace-all (working-nodes selector-or-nodes)
  "Replace each target element with the set of matched elements."
  (let ((targets (nodes-or-select selector-or-nodes)))
    (nodefun-after targets working-nodes)
    (nodefun-remove targets)
    working-nodes))

(define-node-list-function replace-with (working-nodes html-or-nodes)
  "Replace each element with the provided new content and return the set of elements that was removed."
  (let ((new-nodes (nodes-or-build html-or-nodes)))
    (nodefun-after working-nodes new-nodes)
    (nodefun-remove working-nodes)
    working-nodes))

(define-node-list-function show (working-nodes)
  "Display the matched elements (short for (css :display 'block'))"
  (nodefun-css working-nodes :display "block"))

(define-node-function siblings (node &optional selector)
  "Get the siblings of each element, optionally filtered by a selector."
  (let ((siblings (remove node (nodefun-children (nodefun-parent node)))))
    (if selector
        (remove-if-not (lambda (node) (css:node-matches? node selector)) siblings)
        siblings)))

(define-node-list-function size (working-nodes)
  "Return the number of elements in the list."
  (nodefun-length working-nodes))

(define-node-list-function slice (working-nodes start &optional end)
  "Reduce the set of matched elements to a subset specified by a range of indices"
  (subseq working-nodes start end))

(define-node-function text (node &optional (text NIL t-s-p) (document *lquery-master-document*))
  "Get the combined text contents of each element, including their descendants. If text is set, all text nodes are removed and a new text node is appended to the end of the node. If text is NIL, all text nodes are removed from the node."
  (unless (dom:document-p document) (setf document (slot-value document 'rune-dom::owner)))
  (if t-s-p
      (if text 
          (progn
            (vector-push-extend 
             (dom:create-text-node document (format NIL "~a" text))
             (setf (slot-value node 'rune-dom::children)
                   (delete-if #'dom:text-node-p (slot-value node 'rune-dom::children))))
            node)
          (progn
            (setf (slot-value node 'rune-dom::children)
                  (delete-if #'dom:text-node-p (slot-value node 'rune-dom::children)))
            node))
      (buildnode:text-of-dom-snippet node #\space)))

(define-node-function toggle-class (node &rest classes)
  "Add or remove one or more classes from each element, depending on their presence within the element."
  (loop for class in classes
     do (if (nodefun-has-class node class)
            (nodefun-remove-class node class)
            (nodefun-add-class node class)))
  node)

(define-node-function unwrap (node)
  "Remove the parents of the set of matched elements from the DOM, leaving the matched elements in their place. The parent is removed if it is empty after unwrapping."
  (let ((parent (dom:parent-node node)))
    (nodefun-insert-before node parent)
    (nodefun-remove node)
    (if (nodefun-is-empty parent)
        (nodefun-remove parent)))
  node)

(define-node-list-function val (working-nodes &optional value)
  "Get the current values or set the value of every matched element. This uses (attr :value val) in the back."
  (if value
      (nodefun-attr working-nodes :value value)
      (nodefun-attr working-nodes :value)))

(define-node-function wrap (node html-or-nodes)
  "Wrap an HTML structure around each element. Note that always the first subnode is chosen."
  (let ((outer-wrapper (nodes-or-build html-or-nodes)))
    (nodefun-prepend (nodefun-deepest outer-wrapper) node)
    (nodefun-replace-all outer-wrapper node))
  node)

(define-node-list-function wrap-all (working-nodes html-or-nodes)
  "Wrap an HTML structure around all elements inside their next (common) parent."
  (loop with wrapper-template = (first (nodes-or-build html-or-nodes))
     with parentmap = (make-hash-table)
     for node in working-nodes
     for parent = (dom:parent-node node)
     do (setf (gethash parent parentmap)
              (append (gethash parent parentmap) (list node))) 
     finally (loop for parent being the hash-keys of parentmap
                for children being the hash-values of parentmap
                for wrapper = (dom:clone-node wrapper-template T)
                for index = (first (sort (nodefun-child-index children) #'<))
                do (buildnode:insert-nodes (nodefun-deepest wrapper) 0 children)
                   (buildnode:insert-nodes parent index wrapper)))
  working-nodes)  

(define-node-function wrap-inner (node html-or-nodes)
  "Wrap an HTML structure around the contents of each element."
  (let ((inner-wrapper (nodes-or-build html-or-nodes)))
    (nodefun-prepend (nodefun-deepest inner-wrapper) (coerce (dom:child-nodes node) 'list))
    (nodefun-empty node)
    (nodefun-append node inner-wrapper))
  node)

(define-node-list-function write-to-file (working-nodes file &key (doctype "html") (if-does-not-exist :CREATE) (if-exists :SUPERSEDE))
  "Write the serialized node to the file. Note that always only the first element is written."
  (with-open-file (stream file :direction :OUTPUT :if-does-not-exist if-does-not-exist :if-exists if-exists)
    (write-string (nodefun-serialize (first working-nodes) :doctype doctype) stream))
  working-nodes)

(define-node-function serialize (node &key (omit-self NIL) (doctype "html"))
  "Serialize the node into a string."
  (labels ((parse (node) (trim (dom:map-document (cxml:make-string-sink :omit-xml-declaration-p T :canonical NIL) node))))
    (concatenate 
     'string
     (if doctype (format nil "<!DOCTYPE ~a>" doctype))
     (if (or (dom:document-p node) omit-self)
         (parse node)
         (let ((clone (dom:clone-node node T))
               (pseudo (first (build-elements "<div></div>"))))
           (buildnode:insert-nodes pseudo 0 clone)
           (parse pseudo))))))

; Urngh. To avoid copying and creating, there seems to be no other choice but to build the root tag ourselves.
(define-node-function serialize2 (node)
  "Serialize the node into a string. Might be faster than serialize(), but builds parts of the string itself."
  (let ((name (dom:node-name node))
        (attrs (mapcar 
                (lambda (attr) (concatenate 'string " " (dom:name attr) "=\"" (dom:value attr) "\"" ))
                (dom:items (dom:attributes node)))))
  (concatenate 
   'string
   "<" name (apply #'concatenate 'string attrs) ">"
   (trim (dom:map-document 
          (cxml:make-string-sink :omit-xml-declaration-p T :canonical NIL) 
          node))
   "</" name ">")))
