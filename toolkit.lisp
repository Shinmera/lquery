#|
 This file is a part of lQuery
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :lquery)

(defvar *lquery-master-document* NIL
  "The master document used at the beginning of a chain.")

(defun make-proper-vector (&key (size 0) initial-element initial-contents (fill-pointer T))
  "Creates a new proper vector."
  (cond
    (initial-element  (make-array size :initial-element initial-element :adjustable T :fill-pointer fill-pointer))
    (initial-contents (make-array size :initial-contents initial-contents :adjustable T :fill-pointer fill-pointer))
    (T                (make-array size :adjustable T :fill-pointer fill-pointer))))

(defgeneric copy-proper-vector (sequence &key transform)
  (:documentation "Copies the sequence into a new proper vector.")
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
    (copy-proper-vector vector))
  (:method ((list list))
    (copy-proper-vector list))
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
