#|
  This file is a part of lQuery
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :lquery-funcs)

(defun trim (string &optional (chars '(#\Space #\Tab #\Newline)))
  (string-trim chars string))

(defun mkstr (&rest args)
  "Concatenates args by printing into string."
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  "Interns the mkstr output/returns as symbol."
  (values (intern (apply #'mkstr args))))

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
  (declare (ignore root))
  selector-or-nodes)

(defmethod nodes-or-select ((selector-or-nodes dom:node) &optional (root *lquery-master-document*))
  (declare (ignore root))
  (list selector-or-nodes))


(defgeneric nodes-or-build (object &optional root)
  (:documentation "Clone the object as a node list or use it to build a new HTML node."))

(defmethod nodes-or-build ((html-or-nodes string) &optional (root *lquery-master-document*))
  (build-elements html-or-nodes root))

(defmethod nodes-or-build ((html-or-nodes list) &optional (root *lquery-master-document*))
  (declare (ignore root))
  (loop for node in html-or-nodes collect (dom:clone-node node T)))

(defmethod nodes-or-build ((html-or-nodes dom:node) &optional (root *lquery-master-document*))
  (declare (ignore root))
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











(define-node-list-function add (working-nodes selector-or-nodes)
  "Add elements to the set of matched elements."
  (append working-nodes (nodes-or-select selector-or-nodes)))

(define-node-function add-class (node &rest classes)
  "Adds the specified class(es) to the set of matched elements."
  (let ((attribute (dom:get-attribute node "class")))
    (loop for class in classes
         do (setf class (assure-attribute class)
                  attribute (concatenate 'string attribute " " class)))
    (dom:set-attribute node "class" (trim attribute))
    node))

(define-node-function after (node html-or-nodes)
  "Insert content (in html-string or node-list form) after each element."
  (buildnode:insert-children 
   (dom:parent-node node) 
   (1+ (nodefun-child-index node))
   (nodes-or-build html-or-nodes))
  node)

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
  (mapc #'(lambda (new-child) (dom:append-child node new-child)) (nodes-or-build html-or-nodes)) 
  node)

(define-node-list-function append-to (working-nodes selector-or-nodes)
  "Insert every element to the end of the target(s)."
  (loop for target in (nodes-or-select selector-or-nodes)
       do (nodefun-append target working-nodes))
  working-nodes)

(define-node-function attr (node &rest pairs)
  "Retrieve or set attributes on a node"
  (case (length pairs)
    (0 (error "Attribute arguments must be one or more attributes or one or more key-value pairs."))
    (1 (dom:get-attribute node (assure-attribute (first pairs))))
    (otherwise 
     (loop for (key val) on pairs by #'cddr
        do (buildnode:set-attribute node key val)
        finally (return node)))))

(define-node-function before (node html-or-nodes)
  "Insert content (in html-string or node-list form) before each element."
  (buildnode:insert-children (dom:parent-node node)
                             (nodefun-child-index node)
                             (nodes-or-build html-or-nodes))
  node)

(define-node-function children (node &optional selector)
  "Get the children of each element, optionally filtered by a selector."
  (if selector
      (css:query selector node)
      (loop for child across (dom:child-nodes node)
           unless (or (dom:text-node-p child) (dom:comment-p child))
           collect child)))

(define-node-function child-index (node)
  "Returns the index of the element within its parent, also counting text nodes. See index() otherwise."
  (loop with children = (dom:child-nodes (dom:parent-node node))
     for child across children
     for i upto (length children)
     until (eql node child)
     finally (return i)))

(define-node-function clone (node)
  "Create a deep copy of the set of matched elements."
  (dom:clone-node node T))

(define-node-function closest (node selector)
  "For each element in the set, get the first element that matches the selector by testing the element itself and traversing up through its ancestors in the DOM tree."
  (if (css:node-matches? node selector)
      node
      (nodefun-closest (dom:parent-node node) selector)))

(define-node-function contains (node string)
  "Select all elements that contain the specified text."
  (if (string-equal
       (trim string)
       (trim (buildnode:text-of-dom-snippet node)))
      node
      NIL))

(define-node-function contents (node)
  "Get the children of each element, including text and comment nodes."
  (coerce (dom:child-nodes node) 'list))

(define-node-function css (node &rest pairs)
  "Retrieve or set css style attributes on a node."
  (let ((css-styles (get-css-styles node)))
    (case (length pairs)
      (0 (error "CSS attribute arugments must be one or more attributes or one or more key-value pairs."))
      (1 (gethash (symb (assure-attribute (first pairs))) css-styles))
      (otherwise
       (if (and (symbolp (first pairs)) (symbolp (second pairs)))
           (loop for key in pairs
              collecting (gethash (symb (assure-attribute key)) css-styles))
           (loop for (key val) on pairs by #'cddr
              do (setf (gethash (symb (assure-attribute key)) css-styles) val)
              finally (progn
                        (set-css-styles node css-styles)
                        (return node))))))))

(define-node-function data (node &rest pairs)
  "Retrieve or set data attributes on a node. This is a convenience method and uses attr in the back."
  (case (length pairs)
    (0 (error "Data attribute arguments must be one or more attributes or one or more key-value pairs."))
    (otherwise (loop for attr in pairs
                  for i = 0 then (1+ i)
                  do (if (symbolp attr)
                         (setf (nth i pairs) (symb "data-" attr)))
                  finally (return (apply #'nodefun-attr node pairs))))))

(define-node-function deepest (node)
  "Returns the innermost (left-bound) child element."
  (let ((children (nodefun-children node)))
    (if (= (length children) 0)
        node
        (nodefun-deepest (first children))))) 

(define-node-function detach (node &optional selector)
  "Removes the node (optionally filtered by the selector) from the document. Alias for remove()"
  (funcall #'nodefun-remove node selector))

(define-node-list-function each (working-nodes fun)
  "Execute the specified function on each element until NIL is returned or all elements have been processed. The original set of elements is always returned."
  (loop for node in working-nodes
       while (funcall fun node))
  working-nodes)

(define-node-function empty (node)
  "Remove all child nodes from the set of matched elements."
  (buildnode:remove-all-children node)
  node)

(define-node-list-function eq (working-nodes index)
  "Reduce the set of matched elements to the one at the specified index"
  (list (nth index working-nodes)))

(define-node-list-function even (working-nodes)
  "Selects even elements."
  (loop for node in working-nodes 
     for i from 1 upto (length working-nodes)
     if (evenp i)
     collect node))

(define-node-function filter (node selector-or-function)
  "Reduce the set of matched elements to those that match the selector or pass the function's test."
  (if (funcall (funcs-or-select selector-or-function) node) node NIL))

(define-node-function find (node selector-or-function &key (test-self NIL))
  "Get the descendants of each element filtered by selector or function."
  (loop for child in (nodefun-children (list node))
       collect (nodefun-find child selector-or-function :test-self T) into matched
       finally (return (if (and test-self (nodefun-filter node selector-or-function))
                           (concatenate 'list (list node) matched)
                           matched))))

(define-node-list-function first (working-nodes)
  "Reduce the set of matched elements to the first in the set."
  (list (first working-nodes)))

(define-node-list-function gt (working-nodes index)
  "Select all elements at a greater than index(0) within the matched set."
  (subseq working-nodes index))

(define-node-function has (node selector-or-nodes)
  "Reduce the set of matched elements to those that have a descendant that matches the selector or element."
  (let ((find-fun (list-or-selector-func selector-or-nodes)))
    (if (alexandria:flatten (nodefun-find node find-fun)) node NIL)))

(define-node-list-function has-class (working-nodes class)
  "Determine whether any of the matched elements are assigned to the given class."
  (let ((class (assure-attribute class)))
    (loop for node in working-nodes
       if (find class
                (split-sequence:split-sequence #\space (dom:get-attribute node "class")) 
                :test #'string-equal)
       return T)))

(define-node-list-function hide (working-nodes )
  "Hide the matched elements (short for (css :display 'none'))."
  (nodefun-css working-nodes :display "none"))

(define-node-function html (node &optional new-content)
  "Get the HTML contents of the elements or set the HTML contents of every matched element."
  (if new-content
      (progn 
        (nodefun-empty node)
        (buildnode:append-nodes node (build-elements (format NIL "~a" new-content)))
        node)
      (nodefun-serialize node :omit-self T :doctype NIL)))

(define-node-function index (node)
  "Find the index of the node within its parent."
  (let ((children (nodefun-children (nodefun-parent node))))
    (loop for i from 0 upto (length children)
         until (equal (nth i children) node)
         finally (return i))))

(define-node-list-function initialize (working-nodes document)
  "Re-initializes lQuery with a new page."
  (list (initialize
         (etypecase document
           (pathname (load-page document))
           (string (parse-html document))))))

(define-node-list-function insert-after (working-nodes selector-or-nodes)
  "Insert every element after the target."
  (nodefun-after (nodes-or-select selector-or-nodes) working-nodes))

(define-node-list-function insert-before (working-nodes selector-or-nodes)
  "Insert every element before the target."
  (nodefun-before (nodes-or-select selector-or-nodes) working-nodes))

(define-node-list-function is (working-nodes selector-or-nodes)
  "Check the current elements against a selector or list of elements and return true if at least one of them matches."
  (let ((find-fun (list-or-selector-func selector-or-nodes)))
    (loop for node in working-nodes
       if (funcall find-fun node)
       return T)))

(define-node-list-function is-empty (working-nodes)
  "Check if the node contains no children and/or only empty (whitespace) text nodes. If it is empty, T is returned, otherwise NIL."
  (not (nodefun-not-empty working-nodes)))

(define-node-list-function last (working-nodes)
  "Reduce the set of matched elements to the final one in the set."
  (last working-nodes))

(define-node-list-function length (working-nodes)
  "Returns the number of elements in the list."
  (length working-nodes))

(define-node-list-function lt (working-nodes index)
  "Select all elements at an index less than the index within the matched set."
  (subseq working-nodes 0 index))

(define-node-list-function map (working-nodes function)
  "Pass each element through a function (which has to accept one argument, the node), returning the list of all results."
  (mapcar function working-nodes))

(define-node-function next (node &optional selector)
  "Get the immediately following sibling of each element (if there is one). If a selector is provided, the sibling is only included if it matches."
  (let ((family (nodefun-children (nodefun-parent node)))
        (index (nodefun-index node)))
    (if (< index (1- (length family)))
        (let ((sibling (nth (1+ index) family)))
          (if (or (not selector) (css:node-matches? sibling selector))
              sibling)))))

(define-node-function next-all (node &optional selector)
  "Get all following siblings of each element. If a selector is provided, the sibling is only included if it matches."
  (let ((family (nodefun-children (nodefun-parent node)))
        (index (nodefun-index node)))
    (if (< index (1- (length family)))
        (let ((family (subseq family (1+ index))))
          (if selector 
              (remove-if-not (lambda (node) (css:node-matches? node selector)) family)
              family)))))

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
  (when (or (not selector) (css:node-matches? node selector))
    (let ((parent (dom:parent-node node)))
      (if parent
          (dom:remove-child parent node)))
    node))

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

(define-node-function text (node &optional text (document *lquery-master-document*))
  "Get the combined text contents of each element, including their descendants. If text is set, all text nodes are removed and a new text node is appended to the end of the node."
  (unless (dom:document-p document) (setf document (slot-value document 'rune-dom::owner)))
  (if text
      (progn
        (vector-push-extend 
         (dom:create-text-node document (format NIL "~a" text))
         (setf (slot-value node 'rune-dom::children)
               (delete-if #'dom:text-node-p (slot-value node 'rune-dom::children))))
        node)
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
