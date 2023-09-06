(in-package #:org.shirakumo.lquery)

(define-lquery-list-function add (working-nodes selector-or-nodes)
  "Add elements to the set of matched elements."
  (vector-append working-nodes (nodes-or-select selector-or-nodes)))

(define-lquery-function add-class (node &rest classes)
  "Adds the specified class(es) to the set of matched elements.

The following types are handled for each class to add:
  NULL    --- No class is added.
  STRING  --- The string is added as a class.
  SYMBOL  --- The symbol name, downcased, is added as a class.
  LIST    --- Add all classes in the list. Each item must be one of
              the above types."
  (setf (plump:attribute node "class")
        (with-output-to-string (stream)
          (let ((prev (plump:attribute node "class")))
            (when prev (write-string prev stream)))
          (dolist (classish classes)
            (dolist (class (if (listp classish) classish (list classish)))
              (write-char #\Space stream)
              (etypecase class
                (string (write-string class stream))
                (symbol (format stream "~(~a~)" (symbol-name class))))))))
  node)

(define-lquery-list-function after (nodes html-or-nodes)
  "Insert content (in html-string or node-list form) after each element."
  (let ((inserts (nodes-or-build html-or-nodes)))
    (loop for node across nodes
          for position = (plump:child-position node)
          do (array-shift (plump:family node) :n (length inserts) :from (1+ position))
             (loop for i from (1+ position)
                   for insert across inserts
                   do (let ((insert (plump:clone-node insert)))
                        (setf (plump:parent insert) node 
                              (aref (plump:family node) i) insert)))))
  nodes)

(defun parent-lists (nodes)
  (loop with parent-lists = (make-array (length nodes))
        for node across nodes
        for i from 0
        do (loop with list = ()
                 for parent = (plump:parent node)
                   then (plump:parent parent)
                 until (plump:root-p parent)
                 do (push parent list)
                 finally (setf (aref parent-lists i) list))
        finally (return parent-lists)))

(define-lquery-list-function ancestor (working-nodes)
  "Find the common ancestor of all elements."
  (loop with parent-lists = (parent-lists working-nodes)
        with previous = NIL
        with current = NIL
        while (when (loop for i from 1 below (length parent-lists)
                          for a = (setf current (pop (aref parent-lists i)))
                            then (pop (aref parent-lists i))
                          for b = (pop (aref parent-lists 0))
                            then a
                          always (and a (eq a b)))
                (setf previous current))
        finally (return (make-proper-vector :size 1 :initial-element previous))))

(define-lquery-list-function append (nodes html-or-nodes)
  "Insert content (in html-string or node-list form) to the end of each element."
  (let ((inserts (nodes-or-build html-or-nodes)))
    (loop for node across nodes
          do (loop for insert across inserts
                   do (plump:append-child node (plump:clone-node insert)))))
  nodes)

(define-lquery-list-function append-to (working-nodes selector-or-nodes)
  "Insert every element to the end of the target(s)."
  (let ((targets (nodes-or-select selector-or-nodes)))
    (lquery-funcs:append targets working-nodes))
  working-nodes)

(define-lquery-function attr (node &rest pairs)
  "Retrieve or set attributes on a node.
The value on a node is turned into a string using PRINC-TO-STRING.
If a value is NIL, the associated attribute is removed."
  (case (length pairs)
    (0 (error "Attribute arguments must be one or more attributes or one or more key-value pairs."))
    (1 (plump:attribute node (assure-attribute (first pairs))))
    (otherwise 
     (loop for (key val) on pairs by #'cddr
           do (if val
                  (setf (plump:attribute node (assure-attribute key))
                        (princ-to-string val))
                  (plump:remove-attribute node (assure-attribute key))))
     node)))

(define-lquery-list-function before (nodes html-or-nodes)
  "Insert content (in html-string or node-list form) before each element."
  (let ((inserts (nodes-or-build html-or-nodes)))
    (loop for node across nodes
          for position = (plump:child-position node)
          do (array-shift (plump:family node) :n (length inserts) :from position)
             (loop for i from position
                   for insert across inserts
                   do (let ((insert (plump:clone-node insert)))
                        (setf (plump:parent insert) node 
                              (aref (plump:family node) i) insert)))))
  nodes)

(define-lquery-list-function children (nodes &optional selector)
  "Get the children of each element, optionally filtered by a selector."
  (loop with result = (make-proper-vector)
        for node across nodes
        do (loop for child across (plump:children node)
                 do (when (and (plump:element-p child)
                               (or (not selector)
                                   (clss:node-matches-p selector child)))
                      (vector-push-extend child result)))
        finally (return result)))

(define-lquery-function child-index (node)
  "Returns the index of the element within its parent, also counting text nodes. See index() otherwise."
  (plump:child-position node))

(define-lquery-function clone (node)
  "Create a deep copy of the set of matched elements."
  (plump:clone-node node))

(define-lquery-function closest (node selector)
  "For each element in the set, get the first element that matches the selector by testing the element itself and traversing up through its ancestors in the DOM tree.
If no matching element can be found the root is entered instead."
  (when (stringp selector)
    (setf selector (clss:parse-selector selector)))
  (loop do (setf node (plump:parent node))
        until (or (plump:root-p node)
                  (clss:node-matches-p selector node)))
  node)

(define-lquery-list-function contains (nodes string)
  "Select all elements that contain the specified text."
  (loop with string = (trim string)
        with result = (make-proper-vector)
        for node across nodes
        do (when (search string (plump:text node))
             (vector-push-extend node result))
        finally (return result)))

(define-lquery-list-function contents (nodes)
  "Get the children of each element, including text and comment nodes."
  (loop with result = (make-proper-vector)
        for node across nodes
        do (loop for child across (plump:children node)
                 do (vector-push-extend child result))
        finally (return result)))

(define-lquery-function css (node &rest pairs)
  "Retrieve or set css style attributes on a node."
  (let ((css-styles (get-css-styles node)))
    (case (length pairs)
      (0 (error "CSS attribute arugments must be one or more attributes or one or more key-value pairs."))
      (1 (gethash (assure-attribute (first pairs)) css-styles))
      (otherwise
       (loop for (key val) on pairs by #'cddr
             do (setf (gethash (assure-attribute key) css-styles) val))
       (set-css-styles node css-styles)
       node))))

(define-lquery-function data (node &rest pairs)
  "Retrieve or set data attributes on a node. This is a convenience method and uses attr in the back."
  (case (length pairs)
    (0 (error "Data attribute arguments must be one or more attributes or one or more key-value pairs."))
    (1 (plump:attribute node (concatenate 'string "data-" (assure-attribute (first pairs)))))
    (otherwise
     (loop for (key val) on pairs by #'cddr
           for name = (concatenate 'string "data-" (assure-attribute key))
           do (if val
                  (setf (plump:attribute node name)
                        (princ-to-string val))
                  (plump:remove-attribute node name)))
     node)))

(define-lquery-function deepest (node)
  "Returns the innermost (left-bound) child element."
  (labels ((r (node)
             (loop with elp = NIL
                   for child across (plump:children node)
                   when (plump:element-p child)
                     do (setf elp child)
                   until elp
                   finally (return (if elp
                                       (r elp)
                                       node)))))
    (r node))) 

(define-lquery-list-function detach (nodes &optional selector)
  "Removes the node (optionally filtered by the selector) from the document. Alias for remove()"
  (lquery-funcs:remove nodes selector))

(define-lquery-list-function each (nodes fun &key replace)
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

(define-lquery-function empty (node)
  "Remove all child nodes from the set of matched elements."
  (plump:clear node))

(define-lquery-function empty-p (node)
  "Check if the node contains no children and/or only empty (whitespace) text nodes. If it is empty, T is returned, otherwise NIL."
  (loop for child across (plump:children node)
        never (or (plump:element-p child)
                  (and (plump:text-node-p child)
                       (< 0 (length (trim (plump:text child))))))))

(define-lquery-list-function eq (working-nodes index)
  "Reduce the set of matched elements to the one at the specified index"
  (setf (aref working-nodes 0) (aref working-nodes index)
        (fill-pointer working-nodes) 1)
  working-nodes)

(define-lquery-list-function even (working-nodes)
  "Selects even elements, 1-indexed"
  (loop for i from 0
        while (< (1+ (* i 2)) (length working-nodes))
        do (setf (aref working-nodes i)
                 (aref working-nodes (1+ (* i 2))))
        finally (setf (fill-pointer working-nodes) i))
  working-nodes)

(define-lquery-list-function filter (nodes selector-or-function)
  "Reduce the set of matched elements to those that match the selector or pass the function's test."
  (replace-vector-if nodes (funcs-or-select selector-or-function)))

(define-lquery-list-function find (nodes selector-or-function &key (test-self NIL))
  "Get the descendants of each element filtered by selector or function."
  (loop with result = (make-proper-vector)
        with func = (funcs-or-select selector-or-function)
        for node across nodes
        do (labels ((r (node)
                      (loop for child across (plump:children node)
                            do (when (plump:element-p child)
                                 (r child)
                                 (when (funcall func child)
                                   (vector-push-extend child result))))))
             (r node))
           (when (and test-self (funcall func node))
             (vector-push-extend node result))
        finally (return result)))

(define-lquery-list-function first (working-nodes)
  "Reduce the set of matched elements to the first in the set."
  (setf (fill-pointer working-nodes) 1)
  working-nodes)

(define-lquery-list-function gt (working-nodes index)
  "Select all elements at a greater than index(0) within the matched set."
  (loop for i from 0 below (- (length working-nodes) index)
        do (setf (aref working-nodes i)
                 (aref working-nodes (+ i index)))
        finally (setf (fill-pointer working-nodes) (- (length working-nodes) index)))
  working-nodes)

(define-lquery-list-function has (nodes selector-or-nodes)
  "Reduce the set of matched elements to those that have a descendant that matches the selector or element."
  (let ((find-fun (nodes-or-selector-func selector-or-nodes)))
    (replace-vector-if nodes #'(lambda (node)
                                 (< 0 (length (lquery-funcs:find node find-fun)))))))

(define-lquery-list-function has-class (working-nodes class)
  "Determine whether any of the matched elements are assigned to the given class."
  (let ((class (assure-attribute class)))
    (loop for node across working-nodes
          if (find class (classes node) :test #'string-equal)
            return T)))

(define-lquery-list-function hide (working-nodes )
  "Hide the matched elements (short for (css \"display\" \"none\"))."
  (lquery-funcs:css working-nodes "display" "none"))

(define-lquery-function html (node &optional new-content)
  "Get the HTML contents of the elements or set the HTML contents of every matched element.
The new content can be either a plump node, root, pathname, or string. If it is none of those,
it is treated as a string via PRINC-TO-STRING"
  (if new-content
      (progn
        (plump:clear node)
        (typecase new-content
          (plump:root (loop for child across (plump:children new-content)
                            do (plump:append-child node child)))
          (plump:node (plump:append-child node new-content))
          (pathname (plump:parse new-content :root node))
          (T (plump:parse (princ-to-string new-content) :root node)))
        node)
      (with-output-to-string (stream)
        (plump:serialize (plump:children node) stream))))

(define-lquery-list-function html-file (working-nodes pathname)
  "Read an HTML file and insert its contents into each element."
  (let ((document (plump:parse pathname)))
    (loop for node across working-nodes
          do (plump:clear node)
             (loop for child across (plump:children document)
                   do (plump:append-child node (plump:clone-node child))))
    working-nodes))

(define-lquery-function index (node)
  "Find the index of the node within its parent."
  (plump:element-position node))

(define-lquery-list-function initialize (working-nodes document)
  "Re-initializes lQuery with a new page."
  (make-proper-vector :size 1 :initial-element (initialize (load-page document))))

(define-lquery-list-function insert-after (working-nodes selector-or-nodes)
  "Insert every element after the target."
  (lquery-funcs:after (nodes-or-select selector-or-nodes) working-nodes))

(define-lquery-list-function insert-before (working-nodes selector-or-nodes)
  "Insert every element before the target."
  (lquery-funcs:before (nodes-or-select selector-or-nodes) working-nodes))

(define-lquery-list-function is (working-nodes selector-or-nodes)
  "Check the current elements against a selector or list of elements and return true if at least one of them matches."
  (let ((find-fun (nodes-or-selector-func selector-or-nodes)))
    (loop for node across working-nodes
          thereis (funcall find-fun node))))

(defun lquery-funcs:is-empty (node)
  "Check if the node contains no children and/or only empty (whitespace) text nodes. If it is empty, T is returned, otherwise NIL.
Alias of EMPTY-P"
  (lquery-funcs:empty-p node))

(define-lquery-list-function last (working-nodes)
  "Reduce the set of matched elements to the final one in the set."
  (when (< 0 (length working-nodes))
    (setf (aref working-nodes 0)
          (aref working-nodes (1- (length working-nodes)))
          (fill-pointer working-nodes) 1))
  working-nodes)

(define-lquery-list-function length (working-nodes)
  "Returns the number of elements in the list."
  (length working-nodes))

(define-lquery-list-function lt (working-nodes index)
  "Select all elements at an index less than the index within the matched set."
  (setf (fill-pointer working-nodes) index)
  working-nodes)

(define-lquery-list-function map (working-nodes function)
  "Pass each element through a function (which has to accept one argument, the node), returning the list of all results."
  (replace-vector working-nodes function))

(define-lquery-list-function map-apply (working-nodes function)
  "Pass each element through a function by apply, returning the vector of all results.
This is commonly useful in combination with COMBINE."
  (replace-vector working-nodes #'(lambda (element) (apply function element))))

(define-lquery-list-function next (nodes &optional selector)
  "Get the immediately following sibling of each element (if there is one). If a selector is provided, the sibling is only included if it matches."
  (when (stringp selector)
    (setf selector (clss:parse-selector selector)))
  (replace-vector-if nodes #'(lambda (sibling)
                               (and sibling (or (not selector) (clss:node-matches-p selector sibling))))
                     :key #'plump:next-element))

(define-lquery-list-function next-all (nodes &optional selector)
  "Get all following siblings of each element. If a selector is provided, the sibling is only included if it matches."
  (when (stringp selector)
    (setf selector (clss:parse-selector selector)))
  (loop with result = (make-proper-vector)
        for node across nodes
        do (loop for i from (1+ (plump:child-position node))
                   below (length (plump:family node))
                 for sibling = (aref (plump:family node) i)
                 do (when (and (plump:element-p sibling)
                               (or (not selector)
                                   (clss:node-matches-p selector sibling)))
                      (vector-push-extend sibling result)))
        finally (return result)))

(define-lquery-list-function next-until (nodes selector-or-nodes)
  "Get all following silings of each element up to (excluding) the element matched by the selector or node list."
  (loop with fun = (nodes-or-selector-func selector-or-nodes)
        with result = (make-proper-vector)
        for node across nodes
        do (loop for i from (1+ (plump:child-position node))
                   below (length (plump:family node))
                 for sibling = (aref (plump:family node) i)
                 until (and (plump:element-p sibling)
                            (funcall fun sibling))
                 do (when (plump:element-p sibling)
                      (vector-push-extend sibling result)))
        finally (return result)))

(define-lquery-list-function node (working-nodes &optional (n 0))
  "Return the specified node (default first) directly, without encompassing it into a vector if it exists. Otherwise return NIL."
  (when (< n (length working-nodes))
    (elt working-nodes n)))

(define-lquery-list-function not (working-nodes selector-or-nodes)
  "Remove matching elements from the working elements."
  (let ((fun (nodes-or-selector-func selector-or-nodes)))
    (replace-vector-if working-nodes (complement fun))))

(define-lquery-function not-empty (node)
  "Check if the node contains no children and/or only empty (whitespace) text nodes. If the node is effectively empty NIL is returned, otherwise T"
  (loop for child across (plump:children node)
        thereis (or (plump:element-p child)
                    (and (plump:text-node-p child)
                         (< 0 (length (trim (plump:text child))))))))

(define-lquery-list-function odd (working-nodes)
  "Select all odd elements from the current set, 1-indexed."
  (loop for i from 0
        while (< (* i 2) (length working-nodes))
        do (setf (aref working-nodes i)
                 (aref working-nodes (* i 2)))
        finally (setf (fill-pointer working-nodes) i))
  working-nodes)

(define-lquery-list-function parent (nodes &optional selector)
  "Get the parent of each element, optionally filtered by a selector."
  (when (stringp selector)
    (setf selector (clss:parse-selector selector)))
  (replace-vector-if nodes #'(lambda (el)
                               (or (not selector) (clss:node-matches-p selector el))) :key #'plump:parent))

(define-lquery-list-function parents (nodes &optional selector)
  "Get the ancestors of each element, optionally filtered by a selector. Closest parent first."
  (when (stringp selector)
    (setf selector (clss:parse-selector selector)))
  (loop with result = (make-proper-vector)
        for node across nodes
        do (loop for parent = (plump:parent node)
                   then (plump:parent parent)
                 until (or (not parent) (plump:root-p parent))
                 do (when (or (not selector) (clss:node-matches-p selector parent))
                      (vector-push-extend parent result)))
        finally (return result)))

(define-lquery-list-function parents-until (nodes selector-or-nodes)
  "Get the ancestors of each element, up to (excluding) the element matched by the selector or node list. Closest parent first"
  (loop with result = (make-proper-vector)
        with func = (nodes-or-selector-func selector-or-nodes)
        for node across nodes
        do (loop for parent = (plump:parent node)
                   then (plump:parent parent)
                 until (or (not parent)
                           (plump:root-p parent)
                           (funcall func parent))
                 do (vector-push-extend parent result))
        finally (return result)))

(define-lquery-list-function prepend (nodes html-or-nodes)
  "Insert content, specified by the parameter, to the beginning of each element."
  (let ((inserts (nodes-or-build html-or-nodes)))
    (loop for node across nodes
          do (loop for insert across inserts
                   do (plump:prepend-child node (plump:clone-node insert)))))
  nodes)

(define-lquery-list-function prepend-to (working-nodes selector-or-nodes)
  "Insert every element to the beginning of the target(s)."
  (let ((targets (nodes-or-select selector-or-nodes)))
    (lquery-funcs:prepend targets working-nodes))
  working-nodes)

(define-lquery-list-function prev (nodes &optional selector)
  "Get the immediately preceding sibling of each element (if there is one). If a selector is provided, the sibling is only included if it matches."
  (when (stringp selector)
    (setf selector (clss:parse-selector selector)))
  (replace-vector-if nodes #'(lambda (sibling)
                               (and sibling (or (not selector) (clss:node-matches-p selector sibling))))
                     :key #'plump:previous-element))

(define-lquery-list-function prev-all (nodes &optional selector)
  "Get all preceeding siblings of each element. If a selector is provided, the sibling is only included if it matches."
  (when (stringp selector)
    (setf selector (clss:parse-selector selector)))
  (loop with result = (make-proper-vector)
        for node across nodes
        do (loop for i downfrom (1- (plump:child-position node))
                   above 0
                 for sibling = (aref (plump:family node) i)
                 do (when (and (plump:element-p sibling)
                               (or (not selector)
                                   (clss:node-matches-p selector sibling)))
                      (vector-push-extend sibling result)))
        finally (return result)))

(define-lquery-list-function prev-until (nodes selector-or-nodes)
  "Get all preceeding silings of each element down to (excluding) the element matched by the selector or node list."
  (loop with fun = (nodes-or-selector-func selector-or-nodes)
        with result = (make-proper-vector)
        for node across nodes
        do (loop for i downfrom (1- (plump:child-position node))
                   above 0
                 for sibling = (aref (plump:family node) i)
                 until (and (plump:element-p sibling)
                            (funcall fun sibling))
                 do (when (plump:element-p sibling)
                      (vector-push-extend sibling result)))
        finally (return result)))

(define-lquery-function remove (node &optional selector)
  "Remove the set of matched elements from the DOM."
  (when (or (not selector) (clss:node-matches-p selector node))
    (plump:remove-child node))
  node)

(define-lquery-function remove-attr (node &rest attributes)
  "Remove attributes from each element."
  (dolist (attr attributes)
    (plump:remove-attribute node (assure-attribute attr)))
  node)

(define-lquery-function remove-class (node &rest classes)
  "Remove classes from each element.

Each class in the list can be of the following types:
  NULL    --- Nothing is done.
  STRING  --- Matching classes by string= are removed.
  SYMBOL  --- Matching classes against the symbol name by string-equal are removed.
  LIST    --- Add all classes in the list. Each item must be one of
              the above types."
  (setf (plump:attribute node "class")
        (let ((existing (classes node)))
          (dolist (classish classes)
            (dolist (class (if (listp classish) classish (list classish)))
              (setf existing
                    (typecase class
                      (string (delete class existing :test #'string=))
                      (symbol (delete (symbol-name class) existing :test #'string-equal))))))
          (format NIL "~{~a~^ ~}" existing)))
  node)

(define-lquery-function remove-data (node &rest data)
  "Remove data attributes from each element. This is a convenience method and uses remove-attr in the back."
  (loop for dat in data
        do (plump:remove-attribute node (concatenate 'string "data-" dat)))
  node)

(define-lquery-function render-text (node)
  "Return the \"rendered\" representation of the text inside the node and its children.

In effect the text is gathered from the component and all of
its children, but transforming the text in such a way that:
- All ASCII white space (Space, Tab, CR, LF) is converted into spaces.
- There are no consecutive spaces.
- There are no spaces at the beginning or end."
  (plump-dom:render-text node))

(define-lquery-list-function replace-all (working-nodes selector-or-nodes)
  "Replace each in the set of matched elements with the current nodes."
  (let ((targets (nodes-or-select selector-or-nodes)))
    (unless (= 0 (length working-nodes))
      (loop for target across targets
            for position = (plump:child-position target)
            for family = (plump:family target)
            do (array-shift family :n (1- (length working-nodes)) :from position)
               (loop for i from 0 below (length working-nodes)
                     do (setf (aref family (+ i position))
                              (aref working-nodes i)))
               (setf (plump:parent target) NIL)))
    working-nodes))

(define-lquery-list-function replace-with (working-nodes html-or-nodes)
  "Replace each element with the provided new content and return the set of elements that was removed."
  (let ((new-nodes (nodes-or-build html-or-nodes)))
    (lquery-funcs:replace-all new-nodes working-nodes)
    working-nodes))

(define-lquery-list-function root (working-nodes)
  "Returns to the root. Essentially traverses up the tree of the first element in the set until the root is reached."
  (if (= 0 (length working-nodes))
      working-nodes
      (loop for child = (aref working-nodes 0)
            then (plump:parent child)
            do (when (or (not (plump:child-node-p child))
                         (not (plump:parent child)))
                 (return (make-proper-vector :size 1 :initial-element child))))))

(define-lquery-list-function show (working-nodes)
  "Display the matched elements (short for (css :display 'block'))"
  (lquery-funcs:css working-nodes "display" "block"))

(define-lquery-list-function siblings (nodes &optional selector)
  "Get the siblings of each element, optionally filtered by a selector."
  (when (stringp selector)
    (setf selector (clss:parse-selector selector)))
  (loop with result = (make-proper-vector)
        for node across nodes
        do (loop for sibling across (plump:family node)
                 unless (or (eq node sibling)
                            (not (plump:element-p sibling))
                            (and selector
                                 (clss:node-matches-p selector sibling)))
                   do (vector-push-extend sibling result))
        finally (return result)))

(define-lquery-list-function size (working-nodes)
  "Return the number of elements in the list."
  (length working-nodes))

(define-lquery-list-function slice (working-nodes start &optional end)
  "Reduce the set of matched elements to a subset specified by a range of indices"
  (unless end (setf end (length working-nodes)))
  (array-shift working-nodes :from start :to end :n (- start) :adjust NIL)
  (setf (fill-pointer working-nodes) (- end start))
  working-nodes)

(define-lquery-function splice (node)
  "Splice the element's contents in place of itself."
  (plump-dom:splice node)
  node)

(define-lquery-function text (node &optional (text NIL t-s-p))
  "Get the combined text contents of each element, including their descendants. If text is set, all text nodes are removed and a new text node is appended to the end of the node. If text is NIL, all direct text nodes are removed from the node. If text is not a string, it is transformed into one by PRINC-TO-STRING."
  (if t-s-p
      (if text 
          (progn
            (replace-vector-if (plump:children node) (complement #'plump:textual-node-p))
            (plump:make-text-node node (typecase text
                                         (plump:node (with-output-to-string (stream)
                                                       (plump:serialize text stream)))
                                         (T (princ-to-string text))))
            node)
          (progn
            (replace-vector-if (plump:children node) (complement #'plump:textual-node-p))
            node))
      (plump:text node)))

(define-lquery-function toggle-class (node &rest classes)
  "Add or remove one or more classes from each element, depending on their presence within the element."
  (let ((list (classes node)))
    (loop for class in classes
          if (member class list :test #'string=)
            collect class into to-remove
          else
            collect class into to-add
          finally (setf (plump:attribute node "class")
                        (with-output-to-string (stream)
                          (format stream "~{~a~^ ~}" to-add)
                          (loop for item in list
                                do (unless (member item to-remove :test #'string=)
                                     (write-char #\Space)
                                     (write-string item stream)))))))
  node)

(define-lquery-function unwrap (node)
  "Remove the parents of the set of matched elements from the DOM, inserting the parents children in place of it."
  (let* ((parent (plump:parent node))
         (pparent (plump:parent parent))
         (parentpos (plump:child-position parent)))
    (array-shift (plump:children pparent) :n (1- (length (plump:children parent))) :from parentpos)
    (loop for child across (plump:children parent)
          for i from parentpos
          do (setf (aref (plump:children pparent) i) child
                   (plump:parent child) pparent))
    (setf (plump:parent parent) NIL
          (fill-pointer (plump:children parent)) 0))
  node)

(define-lquery-function val (node &optional (value NIL v-p))
  "Get the current values or set the value of every matched element."
  (if v-p
      (progn
        (if value
            (setf (plump:attribute node "value")
                  (princ-to-string value))
            (plump:remove-attribute node "value"))
        node)
      (plump:attribute node "value")))

(define-lquery-list-function wrap (nodes html-or-nodes)
  "Wrap an HTML structure around each element. Note that always the first node of the structure to wrap is chosen."
  (let ((base (aref (nodes-or-build html-or-nodes) 0)))
    (loop for node across nodes
          for wrapper = (plump:clone-node base)
          do (setf (aref (plump:family node) (plump:child-position node)) wrapper)
             (setf (plump:parent wrapper) (plump:parent node))
             (plump:append-child wrapper node)))
  nodes)

(define-lquery-list-function wrap-all (working-nodes html-or-nodes)
  "Wrap an HTML structure around all elements and put it in place of the first element, removing all other elements from their position."
  (let* ((first (aref working-nodes 0))
         (parent (plump:parent first))
         (position (plump:child-position first))
         (wrapper (aref (nodes-or-build html-or-nodes) 0)))
    (plump:append-child wrapper first)
    (loop for i from 1 below (length working-nodes)
          for node = (aref working-nodes i)
          do (plump:remove-child node)
             (plump:append-child wrapper node))
    (setf (aref (plump:children parent) position) wrapper
          (plump:parent wrapper) parent))
  working-nodes)  

(define-lquery-list-function wrap-inner (nodes html-or-nodes)
  "Wrap an HTML structure around the contents of each element."
  (let ((base (aref (nodes-or-build html-or-nodes) 0)))
    (loop for node across nodes
          for wrapper = (plump:clone-node base)
          do (loop for child across (plump:children node)
                   do (setf (plump:parent child) wrapper)
                      (vector-push-extend child (plump:children wrapper)))
             (setf (plump:children node) (make-proper-vector :size 1 :initial-element wrapper)
                   (plump:parent wrapper) node)))
  nodes)

(define-lquery-list-function write-to-file (working-nodes file &key (if-does-not-exist :CREATE) (if-exists :SUPERSEDE))
  "Write the serialized node to the file. Note that always only the first element is written."
  (with-open-file (stream file :direction :OUTPUT :if-does-not-exist if-does-not-exist :if-exists if-exists)
    (plump:serialize (aref working-nodes 0) stream))
  working-nodes)

(define-lquery-function serialize (node &optional (stream NIL) (format :default))
  "Serialize the node into a string.

Allows two optional arguments:
  STREAM --- NIL to return a string, or a stream to output to.
  FORMAT --- One of :DEFAULT, :HTML, :XML to designate the way
             in which to invoke Plump's serializer."
  (let ((plump:*tag-dispatchers* (case format
                                   (:default plump:*tag-dispatchers*)
                                   (:html plump:*html-tags*)
                                   (:xml plump:*xml-tags*))))
    (if stream
        (plump:serialize node stream)
        (with-output-to-string (stream)
          (plump:serialize node stream)))))
