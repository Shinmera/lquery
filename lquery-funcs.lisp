#|
  This file is a part of lQuery
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :lquery-funcs)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(lquery::make-proper-vector
            lquery::copy-proper-vector)))

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

(defgeneric nodes-or-selector-func (object)
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
                  do (format s "~a:~a;" (assure-attribute key) val)))))

(defun replace-vector-if (vector condition &key (key #'identity))
  (loop with i = 0
        for item across vector
        for element = (funcall key item)
        do (when (funcall condition element)
             (setf (aref vector i) element)
             (incf i))
        finally (setf (fill-pointer vector) i))
  vector)

(define-node-list-function add (working-nodes selector-or-nodes)
  "Add elements to the set of matched elements."
  (plump::vector-append working-nodes (nodes-or-select selector-or-nodes)))

(define-node-function add-class (node &rest classes)
  "Adds the specified class(es) to the set of matched elements."
  (setf (plump:attribute node "class")
        (with-output-to-string (stream)
          (format stream "~@[~a ~]~{~a~^ ~}" (plump:attribute node "class") classes)))
  node)

(define-node-list-function after (nodes html-or-nodes)
  "Insert content (in html-string or node-list form) after each element."
  (let ((inserts (nodes-or-build html-or-nodes)))
    (loop for node across nodes
          for position = (plump:child-position node)
          do (plump::array-shift (plump:family node) :n (length inserts) :from (1+ position))
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

(define-node-list-function ancestor (working-nodes)
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

(define-node-list-function append (nodes html-or-nodes)
  "Insert content (in html-string or node-list form) to the end of each element."
  (let ((inserts (nodes-or-build html-or-nodes)))
    (loop for node across nodes
          do (loop for insert across inserts
                   do (plump:append-child node (plump:clone-node insert)))))
  nodes)

(define-node-list-function append-to (working-nodes selector-or-nodes)
  "Insert every element to the end of the target(s)."
  (let ((targets (nodes-or-select selector-or-nodes)))
    (nodefun-append targets working-nodes))
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

(define-node-list-function before (nodes html-or-nodes)
  "Insert content (in html-string or node-list form) before each element."
  (let ((inserts (nodes-or-build html-or-nodes)))
    (loop for node across nodes
          for position = (plump:child-position node)
          do (plump::array-shift (plump:family node) :n (length inserts) :from position)
             (loop for i from position
                   for insert across inserts
                   do (let ((insert (plump:clone-node insert)))
                        (setf (plump:parent insert) node 
                              (aref (plump:family node) i) insert)))))
  nodes)

(define-node-list-function children (nodes &optional selector)
  "Get the children of each element, optionally filtered by a selector."
  (loop with result = (make-proper-vector)
        for node across nodes
        do (loop for child across (plump:children node)
                 do (when (and (plump:element-p child)
                               (or (not selector)
                                   (clss:node-matches-p selector child)))
                      (vector-push-extend child result)))
        finally (return result)))

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
  (loop do (setf node (plump:parent node))
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
       (loop for (key val) on pairs by #'cddr
             do (setf (gethash (assure-attribute key) css-styles) val))
       (set-css-styles node css-styles)
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
  (replace-vector-if nodes (funcs-or-select selector-or-function)))

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
  (setf (fill-pointer working-nodes) 1)
  working-nodes)

(define-node-list-function gt (working-nodes index)
  "Select all elements at a greater than index(0) within the matched set."
  (loop for i from 0 below (- (length working-nodes) index)
        do (setf (aref working-nodes i)
                 (aref working-nodes (+ i index)))
        finally (setf (fill-pointer working-nodes) (- (length working-nodes) index)))
  working-nodes)

(define-node-list-function has (nodes selector-or-nodes)
  "Reduce the set of matched elements to those that have a descendant that matches the selector or element."
  (let ((find-fun (nodes-or-selector-func selector-or-nodes)))
    (replace-vector-if nodes #'(lambda (node)
                                 (< 0 (length (nodefun-find node find-fun)))))))

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
  (let ((find-fun (nodes-or-selector-func selector-or-nodes)))
    (loop for node across working-nodes
          thereis (funcall find-fun node))))

(define-node-function is-empty (node)
  "Check if the node contains no children and/or only empty (whitespace) text nodes. If it is empty, T is returned, otherwise NIL."
  (loop for child across (plump:children node)
        never (or (plump:element-p child)
                  (and (plump:text-node-p child)
                       (< 0 (length (trim (plump:text child))))))))

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
  (replace-vector-if working-nodes #'(lambda (a) (declare (ignore a)) T) :key function))

(define-node-list-function next (nodes &optional selector)
  "Get the immediately following sibling of each element (if there is one). If a selector is provided, the sibling is only included if it matches."
  (when (stringp selector)
    (setf selector (clss:parse-selector selector)))
  (replace-vector-if nodes #'(lambda (sibling)
                            (and sibling (or (not selector) (clss:node-matches-p selector sibling))))
                  :key #'plump:next-element))

(define-node-list-function next-all (nodes &optional selector)
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

(define-node-list-function next-until (nodes selector-or-nodes)
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

(define-node-list-function node (working-nodes &optional (n 0))
  "Return the specified node (default first) directly, without encompassing it into a list."
  (elt working-nodes n))

(define-node-list-function not (working-nodes selector-or-nodes)
  "Remove matching elements from the working elements."
  (let ((fun (nodes-or-selector-func selector-or-nodes)))
    (replace-vector-if working-nodes fun)))

(define-node-function not-empty (node)
  "Check if the node contains no children and/or only empty (whitespace) text nodes. If the node is effectively empty NIL is returned, otherwise T"
  (loop for child across (plump:children node)
        thereis (or (plump:element-p child)
                    (and (plump:text-node-p child)
                         (< 0 (length (trim (plump:text child))))))))

(define-node-list-function odd (working-nodes)
  "Select all odd elements from the current set, 1-indexed."
  (loop for i from 0
        while (< (* i 2) (length working-nodes))
        do (setf (aref working-nodes i)
                 (aref working-nodes (* i 2)))
        finally (setf (fill-pointer working-nodes) i))
  working-nodes)

(define-node-list-function parent (nodes &optional selector)
  "Get the parent of each element, optionally filtered by a selector."
  (when (stringp selector)
    (setf selector (clss:parse-selector selector)))
  (replace-vector-if nodes #'(lambda (el)
                               (or (not selector) (clss:node-matches-p selector el))) :key #'plump:parent))

(define-node-list-function parents (nodes &optional selector)
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

(define-node-list-function parents-until (nodes selector-or-nodes)
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

(define-node-list-function prepend (nodes html-or-nodes)
  "Insert content, specified by the parameter, to the beginning of each element."
  (let ((inserts (nodes-or-build html-or-nodes)))
    (loop for node across nodes
          do (loop for insert across inserts
                   do (plump:prepend-child node (plump:clone-node insert)))))
  nodes)

(define-node-list-function prepend-to (working-nodes selector-or-nodes)
  "Insert every element to the beginning of the target(s)."
  (let ((targets (nodes-or-select selector-or-nodes)))
    (nodefun-prepend targets working-nodes))
  working-nodes)

(define-node-list-function prev (nodes &optional selector)
  "Get the immediately preceding sibling of each element (if there is one). If a selector is provided, the sibling is only included if it matches."
  (when (stringp selector)
    (setf selector (clss:parse-selector selector)))
  (replace-vector-if nodes #'(lambda (sibling)
                               (and sibling (or (not selector) (clss:node-matches-p selector sibling))))
                     :key #'plump:previous-element))

(define-node-list-function prev-all (nodes &optional selector)
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

(define-node-list-function prev-until (nodes selector-or-nodes)
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

(define-node-function remove (node &optional selector)
  "Remove the set of matched elements from the DOM."
  (when (or (not selector) (clss:node-matches-p selector node))
    (plump:remove-child node)))

(define-node-function remove-attr (node &rest attributes)
  "Remove attributes from each element."
  (dolist (attr attributes)
    (plump:remove-attribute node (assure-attribute attr)))
  node)

(define-node-function remove-class (node &rest classes)
  "Remove classes from each element."
  (setf (plump:attribute node "class")
        (format NIL "~{~a~^ ~}"
                (remove-if #'(lambda (a) (find a classes :test #'string=))
                           (split-sequence #\Space (plump:attribute node "class") :remove-empty-subseqs T))))
  node)

(define-node-function remove-data (node &rest data)
  "Remove data attributes from each element. This is a convenience method and uses remove-attr in the back."
  (loop for dat in data
        do (plump:remove-attribute node (concatenate 'string "data-" dat)))
  node)

(define-node-list-function replace-all (working-nodes selector-or-nodes)
  "Replace each in the set of matched elements with the current nodes."
  (let ((targets (nodes-or-select selector-or-nodes)))
    (unless (= 0 (length working-nodes))
      (loop for target across targets
            for position = (plump:child-position target)
            for family = (plump:family target)
            do (plump::array-shift family :n (1- (length working-nodes)) :from position)
               (loop for i from 0 below (length working-nodes)
                     do (setf (aref family (+ i position))
                              (aref working-nodes i)))
               (setf (plump:parent target) NIL)))
    working-nodes))

(define-node-list-function replace-with (working-nodes html-or-nodes)
  "Replace each element with the provided new content and return the set of elements that was removed."
  (let ((new-nodes (nodes-or-build html-or-nodes)))
    (nodefun-replace-all new-nodes working-nodes)
    working-nodes))

(define-node-list-function show (working-nodes)
  "Display the matched elements (short for (css :display 'block'))"
  (nodefun-css working-nodes "show" "block"))

(define-node-list-function siblings (nodes &optional selector)
  "Get the siblings of each element, optionally filtered by a selector."
  (when (stringp selector)
    (setf selector (clss:parse-selector selector)))
  (loop with result = (make-proper-vector)
        for node across nodes
        do (loop for sibling across (plump:family node)
                 unless (or (eq node sibling)
                            (and selector
                                 (clss:node-matches-p selector sibling)))
                   do (vector-push-extend sibling result))
        finally (return result)))

(define-node-list-function size (working-nodes)
  "Return the number of elements in the list."
  (length working-nodes))

(define-node-list-function slice (working-nodes start &optional end)
  "Reduce the set of matched elements to a subset specified by a range of indices"
  (unless end (setf end (length working-nodes)))
  (plump::array-shift working-nodes :from start :to end :n (- start) :adjust NIL)
  (setf (fill-pointer working-nodes) (- end start))
  working-nodes)

(define-node-function text (node &optional (text NIL t-s-p))
  "Get the combined text contents of each element, including their descendants. If text is set, all text nodes are removed and a new text node is appended to the end of the node. If text is NIL, all direct text nodes are removed from the node."
  (if t-s-p
      (if text 
          (progn
            (replace-vector-if (plump:children node) #'(lambda (el) (not (plump:text-node-p el))))
            (plump:make-text-node node text)
            node)
          (progn
            (replace-vector-if (plump:children node) #'(lambda (el) (not (plump:text-node-p el))))
            node))
      (plump:text node)))

(define-node-function toggle-class (node &rest classes)
  "Add or remove one or more classes from each element, depending on their presence within the element."
  (let ((list (split-sequence #\Space (plump:attribute node "class") :remove-empty-subseqs T)))
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

(define-node-function unwrap (node)
  "Remove the parents of the set of matched elements from the DOM, leaving the matched elements in their place. The parent is removed if it is empty after unwrapping."
  (let ((parent (plump:parent node)))
    (nodefun-insert-before node parent)
    (nodefun-remove node)
    (if (nodefun-is-empty parent)
        (nodefun-remove parent)))
  node)

(define-node-function val (node &optional (value NIL v-p))
  "Get the current values or set the value of every matched element."
  (if v-p
      (if value
          (setf (plump:attribute node "value") value)
          (plump:remove-attribute node "value"))
      (plump:attribute node "value")))

(define-node-list-function wrap (nodes html-or-nodes)
  "Wrap an HTML structure around each element. Note that always the first node of the structure to wrap is chosen."
  (let ((base (aref (nodes-or-build html-or-nodes) 0)))
    (loop for node across nodes
          for wrapper = (plump:clone-node base)
          do (setf (aref (plump:family node) (plump:child-position node)) wrapper)
             (plump:append-child wrapper node)))
  nodes)

(define-node-list-function wrap-all (working-nodes html-or-nodes)
  "Wrap an HTML structure around all elements inside their next (common) ancestor."
  (let ((parent (nodefun-ancestor working-nodes))
        (wrapper (aref (nodes-or-build html-or-nodes) 0)))
    (setf (aref (plump:family parent) (plump:child-position parent)) wrapper)
    (plump:append-child wrapper parent))
  working-nodes)  

(define-node-list-function wrap-inner (nodes html-or-nodes)
  "Wrap an HTML structure around the contents of each element."
  (let ((base (aref (nodes-or-build html-or-nodes) 0)))
    (loop for node across nodes
          for wrapper = (plump:clone-node base)
          do (loop for child across (plump:children node)
                   do (setf (plump:parent child) wrapper)
                      (vector-push-extend child wrapper))
             (setf (plump:children node) (make-proper-vector :size 1 :initial-element wrapper)
                   (plump:parent wrapper) node)))
  nodes)

(define-node-list-function write-to-file (working-nodes file &key (if-does-not-exist :CREATE) (if-exists :SUPERSEDE))
  "Write the serialized node to the file. Note that always only the first element is written."
  (with-open-file (stream file :direction :OUTPUT :if-does-not-exist if-does-not-exist :if-exists if-exists)
    (plump:serialize (aref working-nodes 0) stream))
  working-nodes)

(define-node-function serialize (node &optional (stream NIL))
                      "Serialize the node into a string."
  (if stream
      (plump:serialize node stream)
      (with-output-to-string (stream)
        (plump:serialize node stream))))
