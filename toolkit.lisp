(in-package #:org.shirakumo.lquery)

(defvar *lquery-master-document* NIL
  "The master document used at the beginning of a chain.")

(defmacro with-master-document ((&optional (doc '*lquery-master-document*)) &body body)
  "Surrounds the body in a binding for the *lquery-master-document* to ensure it does not get clobbered."
  `(let ((*lquery-master-document* ,doc))
     ,@body))

(defun make-proper-vector (&key (size 0) initial-element initial-contents (fill-pointer T))
  "Creates a new proper vector."
  (cond
    (initial-element  (make-array size :initial-element initial-element :adjustable T :fill-pointer fill-pointer))
    (initial-contents (make-array size :initial-contents initial-contents :adjustable T :fill-pointer fill-pointer))
    (T                (make-array size :adjustable T :fill-pointer fill-pointer))))

(defun copy-proper-vector (sequence &key (transform #'identity))
  "Copies the sequence into a new proper vector."
  (declare (optimize (speed 3)))
  (etypecase sequence
    (vector
     (loop with result = (make-proper-vector :size (length sequence) :fill-pointer T)
           for i from 0 below (length sequence)
           do (setf (aref result i)
                    (funcall transform (aref sequence i)))
           finally (return result)))
    (list
     (loop with length = (length sequence)
           with result = (make-proper-vector :size length :fill-pointer T)
           for i from 0 below length
           for item in sequence
           do (setf (aref result i)
                    (funcall transform item))
           finally (return result)))))

(defun ensure-proper-vector (var)
  "Ensure that the variable is a proper vector."
  (typecase var
    (vector (if (adjustable-array-p var)
                var
                (copy-proper-vector var)))
    (array (copy-proper-vector var))
    (list (copy-proper-vector var))
    (T (make-proper-vector :size 1 :initial-element var))))

(defun load-page (file-or-string)
  "Load the given file or string into a HTML DOM."
  (plump:parse file-or-string))

(defun initialize (document)
  "Sets the *lquery-master-document* variable to the provided document."
  (setf *lquery-master-document* document)
  document)

(defun parse-html (html)
  "Build the given string into DOM objects related to the master document."
  (build-elements html))

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

(defun nodes-or-select (object &optional (root *lquery-master-document*))
  "Return the object as a node list or use it to form a select query."
  (declare (optimize (speed 3)))
  (etypecase object
    (string (clss:select object root))
    (vector object)
    (list (copy-proper-vector object))
    (plump:node (make-proper-vector :size 1 :initial-element object))))

(defun nodes-or-build (object)
  "Clone the object as a node list or use it to build a new HTML node."
  (declare (optimize (speed 3)))
  (etypecase object
    (string (build-elements object))
    (sequence (copy-proper-vector object))
    (plump:node (make-proper-vector :size 1 :initial-element (plump:clone-node object)))))

(defun funcs-or-select (object)
  "Return the object as a function or use it to construct a node-matches? function."
  (declare (optimize (speed 3)))
  (etypecase object
    (string
     (let ((selector (clss:parse-selector object)))
       #'(lambda (node)
           (clss:node-matches-p selector node))))
    (function
     object)))

(defun nodes-or-selector-func (object)
  "Build a function matching the selector or checking the equality/inclusion of the object."
  (declare (optimize (speed 3)))
  (etypecase object
    (string
     (let ((selector (clss:parse-selector object)))
       #'(lambda (node)
           (clss:node-matches-p selector node))))
    (list
     #'(lambda (node) (find node object)))
    (plump:node
     #'(lambda (node) (eql node object)))))

(defun classes (node)
  (let ((attr (plump:attribute node "class")))
    (when attr
      (let ((out (make-string-output-stream))
            (parts ()))
        (flet ((maybe-add-piece ()
                 (let ((str (get-output-stream-string out)))
                   (when (and str (string/= str ""))
                     (push str parts)))))
          (loop for char across attr
                do (case char
                     ((#\Space #\Tab #\Return #\Linefeed)
                      (maybe-add-piece))
                     (T
                      (write-char char out)))
                finally (maybe-add-piece))
          (nreverse parts))))))

(defun parse-css (css)
  (macrolet ((pop-string (stream)
               `(prog1 (get-output-stream-string ,stream)
                  (close ,stream)
                  (setf ,stream (make-string-output-stream)))))
    (loop with table = (make-hash-table :test 'equalp)
          with output = (make-string-output-stream)
          with attribute = NIL
          with section = :name
          for char across css
          do (case section
               (:name (case char
                        (#\: (setf section :value
                                   attribute (pop-string output)))
                        (#\Space)
                        (T (write-char char output))))
               (:value (case char
                         (#\; (setf section :name
                                    (gethash attribute table) (pop-string output)))
                         (T (write-char char output)
                          (case char
                            (#\( (setf section #\)))
                            (#\" (setf section #\"))))))
               (T (write-char char output)
                (when (char= char section)
                  (setf section :value))))
          finally (progn
                    (unless (eql section :name)
                      (setf (gethash attribute table) (pop-string output)))
                    (return table)))))

(defun get-css-styles (node)
  (parse-css (or (plump:attribute node "style") "")))

(defun set-css-styles (node css-styles)
  (setf (plump:attribute node "style")
        (with-output-to-string (s)
          (loop for key being the hash-keys of css-styles
                for val being the hash-values of css-styles
                unless (= (length val) 0)
                  do (format s "~a:~a;" (assure-attribute key) val)))))

(defun replace-vector (vector function)
  (loop for i from 0 below (length vector)
        do (setf (aref vector i)
                 (funcall function (aref vector i))))
  vector)

(defun replace-vector-if (vector condition &key (key #'identity))
  (loop with i = 0
        for item across vector
        for element = (funcall key item)
        do (when (funcall condition element)
             (setf (aref vector i) element)
             (incf i))
        finally (setf (fill-pointer vector) i))
  vector)
