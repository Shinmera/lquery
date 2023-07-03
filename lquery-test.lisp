(in-package #:cl-user)
(defpackage #:lquery-test
  (:use #:cl #:lquery #:5am)
  (:shadow #:run)
  (:nicknames #:org.shirakumo.lquery.test)
  (:export #:lquery #:run))

(in-package #:org.shirakumo.lquery.test)

(defvar *test-file* (merge-pathnames "test.html" (asdf:system-source-directory :lquery)))

(defun init-test-environment (&optional (testfile *test-file*))
  "Initializes lQuery with a test HTML file."
  (lquery:initialize (lquery:load-page testfile)))

(defun run ()
  (5am:run! 'lquery))

(defmethod asdf:perform ((op asdf:test-op) (sys (eql (asdf:find-system :lquery-test))))
  (run))

(defun vlast (vector)
  (if (= 0 (length vector))
      NIL
      (aref vector (1- (length vector)))))

(defun vfirst (vector)
  (if (= 0 (length vector))
      NIL
      (aref vector 0)))

(defun vsecond (vector)
  (if (< (length vector) 1)
      NIL
      (aref vector 1)))

(def-suite lquery)
(in-suite lquery)

(test syntax
  "Test the base syntax macro ($)."
  (init-test-environment)
  (is (vectorp ($)))
  (is (plump:root-p (vfirst ($))))
  (is (plump:element-p (vfirst ($ "html"))))
  (is (string-equal "html" (plump:tag-name (vfirst ($ "html")))))
  (is (plump:node-p (vfirst ($ "#source"))))
  (is (= 2
         (length ($ ".fixed"))))
  (is (= 2
         (length ($ "article header, article blockquote"))))
  (is (eq (vfirst ($ "ul"))
          (vfirst ($ "#list"))))
  (is (eq (vfirst ($ "ul"))
          (let ((selector "#list"))
            (vfirst ($ selector)))))
  (is (eq T
          (labels ((root-p (nodes) (plump:root-p (vfirst nodes))))
            ($ #'root-p))))
  (is (typep ($ ()) 'vector))
  (is (eq T
          ($ (inline T))))
  (is (eq (vfirst ($ "ul"))
          (vfirst ($ (inline "ul")))))
  (is (eq (vfirst ($ "ul"))
          (vfirst ($ (eval (format NIL "ul")))))))

(test fun-add
  (init-test-environment)
  (is (= (+ (length ($ "li")) (length ($ "p")))
         (length ($ "li" (add "p")))))
  (is (= (+ (length ($ "head")) (length ($ "body")))
         (length ($ "body" (add "head")))))
  (is (eq (vfirst ($ "article"))
          (vlast ($ "li" (add "article"))))))

(test fun-add-class
  (init-test-environment)
  (is (string-equal "foo"
                    (plump:attribute (vfirst ($ "article" (add-class "foo"))) "class")))
  (is (string-equal "foo bar"
                    (plump:attribute (vfirst ($ "article" (add-class "bar"))) "class")))
  (is (string-equal (format NIL "~a ~a" (plump:get-attribute (vfirst ($ "div")) "class") "baz")
                    (plump:attribute (vfirst ($ "div" (add-class "baz"))) "class"))))

(test (fun-after :depends-on (AND . (fun-children fun-child-index)))
  (init-test-environment)
  (is (eq (vfirst ($ "article blockquote p"))
          (vfirst ($ "article blockquote p" (after "<div>Foo</div>")))))
  (is (= 2
         (length ($ "article blockquote" (children)))))
  (is (string-equal "div"
                    (plump:tag-name (vlast ($ "article blockquote" (children)))))))

(test fun-ancestor
  (init-test-environment)
  (is (eq (vfirst ($ "article"))
          (vfirst ($ "article header, article blockquote" (ancestor)))))
  (is (eq (vfirst ($ "#source"))
          (vfirst ($ "article p, #source p" (ancestor))))))

(test (fun-append :depends-on (AND . (fun-children)))
  (init-test-environment)
  (is (eq (vfirst ($ "#target"))
          (vfirst ($ "#target" (append ($ "#source article"))))))
  (is (= 1
         (length ($ "#target" (children)))))
  (is (string-equal "article"
                    (plump:tag-name (vfirst ($ "#target article")))))
  (is (= 2
         (length ($ "#target article" (children))))))

(test (fun-append-to :depends-on (AND . (fun-children fun-append)))
  (init-test-environment)
  (is (eq (vfirst ($ "#source article"))
          (vfirst ($ "#source article" (append-to "#target")))))
  (is (= 1
         (length ($ "#target" (children)))))
  (is (string-equal "article"
                    (plump:tag-name (vfirst ($ "#target article")))))
  (is (= 2
         (length ($ "#target article" (children))))))

(test fun-attr
  (init-test-environment)
  (is (string-equal "list"
                    (vfirst ($ "#source ul" (attr :id)))))
  (is (eq (vfirst ($ "#source article"))
          (vfirst ($ "#source article" (attr :foo "bar")))))
  (is (string-equal (plump:get-attribute (vfirst ($ "#source article")) "foo")
                    (vfirst ($ "#source article" (attr :foo))))))

(test (fun-before :depends-on (AND . (fun-children fun-child-index)))
  (init-test-environment)
  (is (eq (vfirst ($ "article blockquote p"))
          (vfirst ($ "article blockquote p" (before "<div>Foo</div>")))))
  (is (= 2
         (length ($ "article blockquote" (children)))))
  (is (string-equal "div"
                    (plump:tag-name (vfirst ($ "article blockquote" (children)))))))

(test fun-children
  (init-test-environment)
  (is (= 3
         (length ($ "ul" (children)))))
  (is (string-equal "p"
          (plump:tag-name (vfirst ($ "#source" (children))))))
  (is-false (> 3
               (length ($ "#source" (children))))))

(test fun-child-index
  (init-test-environment)
  (is (= 1
         (vfirst ($ "h1" (child-index)))))
  (is (= 1
         (vfirst ($ "#source>p" (child-index)))))
  (is (= 3
         (vfirst ($ "#source>ul" (child-index))))))

(test (fun-clone :depends-on (AND . (fun-serialize)))
  (init-test-environment)
  (is-false (eq (vfirst ($ "article"))
                (vfirst ($ "article" (clone)))))
  (is (string-equal (vfirst ($ "article" (serialize)))
                    (vfirst ($ "article" (clone) (serialize))))))

(test fun-closest
  (init-test-environment)
  (is (eq (vfirst ($ "div"))
          (vfirst ($ "article p" (closest "#source"))))))

(test fun-contains
  (init-test-environment)
  (is (eq (vfirst ($ "title"))
          (vfirst ($ "body, #target, title" (contains "lQuery Test")))))
  (is (eq (vfirst ($ "#target"))
          (vfirst ($ "body, #target, title" (contains "")))))
  (is-false (eq (vfirst ($ "body"))
                (vfirst ($ "body, #target, title" (contains "Foo"))))))

(test fun-contents
  (init-test-environment)
  (is (= 7
         (length ($ "#list" (contents)))))
  (is (= 1
         (length ($ "title" (contents)))))
  (is-false (eq (vfirst ($ "title"))
                (vfirst ($ "head" (contents))))))

(test fun-css
  (init-test-environment)
  (is (eq (vfirst ($ "#target"))
          (vfirst ($ "#target" (css :color "blue")))))
  (is (string-equal "color:blue;"
                    (plump:get-attribute (vfirst ($ "#target")) "style")))
  (is (string-equal "blue"
                    (vfirst ($ "#target" (css :color)))))
  (is (string-equal "color:red;"
                    (plump:get-attribute (vfirst ($ "#target" (css :color "red"))) "style")))
  (is (string-equal "color:red;foo:bar;"
                    (plump:get-attribute (vfirst ($ "#target" (css :foo "bar"))) "style")))
  (is (string-equal "color:blue;"
                    (plump:get-attribute (vfirst ($ "#target" (css :foo "" :color "blue"))) "style"))))

(test (fun-data :depends-on (AND . (fun-attr)))
  (init-test-environment)
  (is (eq (vfirst ($ "#target"))
          (vfirst ($ "#target" (data :foo "bar")))))
  (is (string-equal "bar"
                    (vfirst ($ "#target" (data :foo))))))

(test (fun-deepest :depends-on (AND . (fun-remove)))
  (init-test-environment)
  (is (eq (vfirst ($ "#source>p"))
          (vfirst ($ "#source" (deepest)))))
  (is (eq (vfirst ($ "title"))
          (vfirst ($ "html" (deepest))))))

(test (fun-detach :depends-on (AND . (fun-remove))))

(test fun-each
  (init-test-environment)
  (is (string-equal "AAAA"
                    (with-output-to-string (s)
                      ($ "li, article, #target" 
                         (each (lambda (node) (progn (format s "A")
                                                     (string-equal "li" (plump:tag-name node)))))))))
  (is (eq (vfirst ($ "li, article, #target"))
          (vfirst ($ "li, article, #target" (each (lambda (node) (string-equal "li" (plump:tag-name node))))))))
  (is (eq (vlast ($ "li, article, #target"))
          (vlast ($ "li, article, #target" (each (lambda (node) (string-equal "li" (plump:tag-name node)))))))))

(test fun-empty
  (init-test-environment)
  (is (= 0
         (length (plump:children (vfirst ($ "article" (empty))))))))

(test fun-eq
  (init-test-environment)
  (is (= 1
         (length ($ "li" (eq 1)))))
  (is (eq (vsecond ($ "li"))
          (vfirst ($ "li" (eq 1))))))

(test fun-even
  (init-test-environment)
  (is (= 1
         (length ($ "li" (even)))))
  (is (eq (vsecond ($ "li"))
          (vfirst ($ "li" (even))))))

(test fun-filter
  (init-test-environment)
  (is (eq (vsecond ($ "head, body"))
          (vfirst ($ "head, body" (filter (lambda (node) (string-equal (plump:tag-name node) "body")))))))
  (is (eq (vsecond ($ "head, body"))
          (vfirst ($ "head, body" (filter "body"))))))

(test (fun-find :depends-on (AND . (fun-children fun-filter)))
  (init-test-environment)
  (is (= 3
         (length ($ "html" (find "li")))))
  (is (eq (vfirst ($ "blockquote p"))
          (vfirst ($ "html" (find "blockquote") (find "p"))))))

(test fun-first
  (init-test-environment)
  (is (= 1
         (length ($ "li" (first)))))
  (is (eq (vfirst ($ "li"))
          (vfirst ($ "li" (first))))))

(test fun-gt
  (init-test-environment)
  (is (= 2
         (length ($ "li" (gt 1)))))
  (is (eq (vsecond ($ "li"))
          (vfirst ($ "li" (gt 1))))))

(test (fun-has :depends-on (AND . (fun-find)))
  (init-test-environment)
  (is (= 2
         (length ($ "head, body, article" (has "p")))))
  (is (eq (vfirst ($ "body"))
          (vfirst ($ "head, body, article" (has "p"))))))

(test fun-has-class
  (init-test-environment)
  (is (eq T ($ "head, body, div" (has-class "fixed"))))
  (is-false (eq T ($ "div" (has-class "foobar")))))

(test (fun-hide :depends-on (AND . (fun-css)))
  (init-test-environment)
  (is (string-equal "display:none;"
                    (plump:get-attribute (vfirst ($ "article" (hide))) "style"))))

(test (fun-html :depends-on (AND . (fun-empty fun-serialize)))
  (init-test-environment)
  (is (eq (vfirst ($ "li"))
          (vfirst ($ "li" (html "FOO")))))
  (is (string-equal "FOO"
                    (vfirst ($ "li" (html))))))

(test (fun-index :depends-on (AND . (fun-children fun-parent)))
  (init-test-environment)
  (is (= 2
         (vfirst ($ "article" (index)))))
  (is (= 0 
         (vfirst ($ "article p" (index))))))

(test fun-initialize
  (lquery:initialize NIL)
  (is (eq (vfirst ($ (initialize *test-file*)))
          (vfirst ($)))))

(test (fun-insert-after :depends-on (AND . (fun-contains fun-after)))
  (init-test-environment)
  (is-false (eq (vfirst ($ "article"))
                (vfirst ($ "article" (insert-after "#target")))))
  (is (= 4
         (length ($ "body" (children)))))
  (is (string-equal "article"
                    (plump:tag-name (vlast ($ "body" (children)))))))

(test (fun-insert-before :depends-on (AND . (fun-contains fun-before)))
  (init-test-environment)
  (is-false (eq (vfirst ($ "article"))
                (vfirst ($ "article" (insert-before "#target")))))
  (is (= 4
         (length ($ "body" (children)))))
  (is (string-equal "article"
                    (plump:tag-name (aref ($ "body" (children)) 2)))))

(test fun-is
  (init-test-environment)
  (is (eq T ($ "head, body, div" (is "#source"))))
  (is-false (eq T ($ "head, body" (is "#source")))))

(test (fun-is-empty)
  (init-test-environment)
  (is (eq NIL (vfirst ($ "title" (is-empty)))))
  (is (eq T (vfirst ($ "#target" (is-empty))))))

(test fun-last
  (init-test-environment)
  (is (= 1
         (length ($ "div" (last)))))
  (is (eq (vfirst ($ "#target"))
          (vfirst ($ "div" (last))))))

(test fun-length
  (init-test-environment)
  (is (= (length ($ "li"))
         ($ "li" (length)))))

(test fun-lt
  (init-test-environment)
  (is (= 3 
         (length ($ "li"))))
  (is (= 2
         (length ($ "li" (lt 2))))))

(test fun-map
  (init-test-environment)
  (is (string-equal "source"
                    (vfirst ($ "div" (map (lambda (node) (plump:get-attribute node "id"))))))))

(test (fun-next :depends-on (AND . (fun-children fun-parent fun-index)))
  (init-test-environment)
  (is (eq (vfirst ($ "#list"))
          (vfirst ($ "#source>p" (next)))))
  (is (eq (vfirst ($ "article blockquote"))
          (vfirst ($ "article header" (next)))))
  (is (= 0 (length ($ "article header" (next "div"))))))

(test (fun-next-all :depends-on (AND . (fun-children fun-parent fun-index fun-first fun-last)))
  (init-test-environment)
  (is (= 2
         (length ($ "#list li" (first) (next-all)))))
  (is (eq (vfirst ($ "#target"))
          (vfirst ($ "h1" (next-all) (last)))))
  (is (eq (vfirst ($ "#list"))
          (vfirst ($ "#source p" (next-all "ul"))))))

(test (fun-next-until :depends-on (AND . (fun-next-all)))
  (init-test-environment)
  (is (= 1
         (length ($ "h1" (next-until "#target")))))
  (is (eq (vfirst ($ "#source"))
          (vlast ($ "h1" (next-until "#target"))))))

(test fun-node
  (init-test-environment)
  (is (eq (vfirst ($ "li"))
          ($ "li" (node))))
  (is (eq (vfirst ($ "#target"))
          ($ "div" (node 1)))))

(test fun-not
  (init-test-environment)
  (is (= 2
         (length ($ "head, body, li" (not "li")))))
  (is (eq (vfirst ($ "head"))
          (vfirst ($ "li, head, body" (not "li"))))))

(test fun-not-empty
  (init-test-environment)
  (is (= 1
         (length ($ "title" (not-empty)))))
  (is (eq NIL
          (vfirst ($ "#target" (not-empty))))))

(test fun-odd
  (init-test-environment)
  (is (= 2
         (length ($ "li" (odd)))))
  (is (eq (vfirst ($ "li"))
          (vfirst ($ "li" (odd))))))

(test fun-parent
  (init-test-environment)
  (is (eq (vfirst ($ "body"))
          (vfirst ($ "h1" (parent)))))
  (is (eq NIL
          (vfirst ($ "h1" (parent "#source"))))))

(test fun-parents
  (init-test-environment)
  (is (eq (vfirst ($ "html"))
          (vlast ($ "article p" (parents)))))
  (is (eq (vfirst ($ "#source"))
          (vlast ($ "article p" (parents "div"))))))

(test fun-parents-until
  (init-test-environment)
  (is (eq (vfirst ($ "#source"))
          (vlast ($ "article p" (parents-until "body"))))))

(test (fun-prepend :depends-on (AND . (fun-children)))
  (init-test-environment)
  (is (eq (vfirst ($ "#list"))
          (vfirst ($ "#list" (prepend ($ "article"))))))
  (is (= 4
         (length ($ "#list" (children)))))
  (is (string-equal "article"
                    (plump:tag-name (vfirst ($ "#list" (children)))))))

(test (fun-prepend-to :depends-on (AND . (fun-children fun-prepend)))
  (init-test-environment)
  (is (eq (vfirst ($ "article"))
          (vfirst ($ "article" (prepend-to ($ "#list"))))))
  (is (= 4
         (length ($ "#list" (children)))))
  (is (string-equal "article"
                    (plump:tag-name (vfirst ($ "#list" (children)))))))

(test (fun-prev :depends-on (AND . (fun-children fun-parent fun-index)))
  (init-test-environment)
  (is (eq (vfirst ($ "#list"))
          (vfirst ($ "article" (prev)))))
  (is (eq (vfirst ($ "article header"))
          (vfirst ($ "article blockquote" (prev)))))
  (is (= 0
         (length ($ "article blockquote" (prev "div"))))))

(test (fun-prev-all :depends-on (AND . (fun-children fun-parent fun-index fun-first fun-last)))
  (init-test-environment)
  (is (= 2
         (length ($ "#list li" (last) (prev-all)))))
  (is (eq (vfirst ($ "h1"))
          (vfirst ($ "#target" (prev-all) (last)))))
  (is (eq (vfirst ($ "#list"))
          (vfirst ($ "article" (prev-all "ul"))))))

(test (fun-prev-until :depends-on (AND . (fun-prev-all)))
  (init-test-environment)
  (is (= 1
         (length ($ "#target" (prev-until "h1")))))
  (is (eq (vfirst ($ "#source"))
          (vlast ($ "#target" (prev-until "h1"))))))

(test (fun-remove :depends-on (AND . (fun-children)))
  (init-test-environment)
  (is (eq (vfirst ($ "li"))
          (vfirst ($ "li" (remove)))))
  (is (= 0 
         (length ($ "#list" (children))))))

(test fun-remove-attr
  (init-test-environment)
  (is (eq (vfirst ($ "#source"))
          (vfirst ($ "#source" (remove-attr "class")))))
  (is (eq NIL (plump:has-attribute (vfirst ($ "#source")) "class"))))

(test (fun-remove-class :depends-on (AND . (fun-has-class fun-add-class)))
  (init-test-environment)
  (is (eq (vfirst ($ "#source"))
          (vfirst ($ "#source" (remove-class "fixed")))))
  (is (eq NIL ($ "#source" (has-class "fixed"))))
  ($ "#source" (add-class "foo" "bar" "baz") (remove-class "bar"))
  (is (eq NIL ($ "#source" (has-class "bar"))))
  (is (eq T ($ "#source" (has-class "baz")))))

(test (fun-remove-data :depends-on (AND . (fun-data fun-remove-attr)))
  (init-test-environment)
  ($ "#source" (data :foo "bar"))
  (is (eq (vfirst ($ "#source"))
          (vfirst ($ "#source" (remove-data "foo")))))
  (is (eq NIL (plump:has-attribute (vfirst ($ "#source")) "data-foo"))))

(test (fun-replace-all :depends-on (AND . (fun-children fun-after fun-remove)))
  (init-test-environment)
  (is (eq (vfirst ($ "article"))
          (vfirst ($ "article" (replace-all "#target")))))
  (is (= 3
         (length ($ "body" (children)))))
  (is (string-equal "article"
                    (plump:tag-name (vlast ($ "body" (children)))))))

(test (fun-replace-with :depends-on (AND . (fun-children fun-after fun-remove)))
  (init-test-environment)
  (is (eq (vfirst ($ "#target"))
          (vfirst ($ "#target" (replace-with ($ "article"))))))
  (is (= 3
         (length ($ "body" (children)))))
  (is (string-equal "article"
                    (plump:tag-name (vlast ($ "body" (children)))))))

(test (fun-show :depends-on (AND . (fun-css)))
  (init-test-environment)
  (is (string-equal "display:block;"
                    (plump:get-attribute (vfirst ($ "article" (show))) "style"))))

(test (fun-siblings :depends-on (AND . (fun-children fun-parent)))
  (init-test-environment)
  (is (= 2
         (length ($ "#source" (siblings)))))
  (is (eq (vfirst ($ "#target"))
          (vsecond ($ "#source" (siblings))))))

(test (fun-size :depends-on (AND . (fun-length)))
  (init-test-environment)
  (is (= (length ($ "li"))
         ($ "li" (size)))))

(test fun-slice
  (init-test-environment)
  (is (= 3
         (length ($ "li, p, div" (slice 1 4)))))
  (is (eq (vsecond ($ "#list li"))
          (vfirst ($ "li, p, div" (slice 1 4))))))

(test fun-splice
  (init-test-environment)
  (is (= (length (plump-dom:children ($ "ul" (node))))
         7))
  ($ "li" (splice))
  (is (= (length (plump-dom:children ($ "ul" (node))))
         7))
  (is (= (length ($ "ul" (children)))
         0)))

(test fun-text
  (init-test-environment)
  (is (eq (vfirst ($ "#source>p"))
          (vfirst ($ "#source>p" (text "FooBar")))))
  (is (string-equal "FooBar"
                    (vfirst ($ "#source>p" (text))))))

(test (fun-toggle-class :depends-on (AND . (fun-has-class fun-remove-class fun-add-class)))
  (init-test-environment)
  (is-false (eq T ($ "article" (has-class "foo"))))
  (is (eq (vfirst ($ "article"))
          (vfirst ($ "article" (toggle-class "foo")))))
  (is (eq T ($ "article" (has-class "foo"))))
  (is (eq (vfirst ($ "article"))
          (vfirst ($ "article" (toggle-class "foo")))))
  (is-false (eq T ($ "article" (has-class "foo")))))

(test fun-unwrap
  (init-test-environment)
  (is (eq (vfirst ($ "#list li:first-child"))
          (vfirst ($ "#list li:first-child" (unwrap)))))
  (is (= 0
         (length ($ "#list"))))
  (is (= 3
         (length ($ "#source li")))))

(test (fun-val :depends-on (AND . (fun-attr)))
  (init-test-environment)
  (is (eq (vfirst ($ "article"))
          (vfirst ($ "article" (val "foo")))))
  (is (string-equal "foo"
                    (vfirst ($ "article" (val))))))

(test (fun-wrap :depends-on (AND . (fun-prepend fun-deepest fun-replace-all)))
  (init-test-environment)
  (is (eq (vfirst ($ "li"))
          (vfirst ($ "li" (wrap "<div></div>")))))
  (is-false (eq (vfirst ($ "#list"))
                (plump:parent (vfirst ($ "li")))))
  (is-false (eq (plump:parent (vfirst ($ "li")))
                (plump:parent (vsecond ($ "li"))))))

(test (fun-wrap-all :depends-on (AND . (fun-child-index fun-deepest)))
  (init-test-environment)
  (is (eq (vfirst ($ "li"))
          (vfirst ($ "li" (wrap-all "<div></div>")))))
  (is-false (eq (vfirst ($ "#list"))
                (plump:parent (vfirst ($ "li")))))
  (is (eq (plump:parent (vfirst ($ "li")))
          (plump:parent (vsecond ($ "li"))))))

(test (fun-wrap-inner :depends-on (AND . (fun-prepend fun-deepest fun-empty fun-append)))
  (init-test-environment)
  (is (eq (vfirst ($ "#list"))
          (vfirst ($ "#list" (wrap-inner "<div></div>")))))
  (is-false (eq (vfirst ($ "#list"))
                (plump:parent (vfirst ($ "li")))))
  (is (eq (plump:parent (vfirst ($ "li")))
          (plump:parent (vsecond ($ "li"))))))

(test (fun-write-to-file)
  (init-test-environment)
  (is (eq (vfirst ($ "article"))
          (vfirst ($ "article" (write-to-file (merge-pathnames "test-output.html" (asdf:system-source-directory :lquery)))))))
  (is (probe-file (merge-pathnames "test-output.html" (asdf:system-source-directory :lquery))))
  (is (delete-file (merge-pathnames "test-output.html" (asdf:system-source-directory :lquery)))))

(test fun-serialize
  (init-test-environment)
  (is (string-equal "<p>Article Text</p>"
                    (vfirst ($ "article p" (serialize))))))
