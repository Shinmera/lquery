#|
  This file is a part of lQuery
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(defpackage :org.tymoonnext.radiance.lib.lquery.test
  (:use :cl :lquery :5am)
  (:nicknames :lquery-test)
  (:export :lquery))

(in-package :org.tymoonnext.radiance.lib.lquery.test)

(defvar *test-file* (merge-pathnames "test.html" (asdf:system-source-directory :lquery)))

(defun init-test-environment (&optional (testfile *test-file*))
  "Initializes lQuery with a test HTML file."
  (lquery:initialize (lquery:load-page testfile)))


(def-suite lquery)
(in-suite lquery)

(test syntax
  "Test the base syntax macro ($)."
  (init-test-environment)
  (is (listp ($)))
  (is (dom:document-p (first ($))))
  (is (dom:node-p (first ($ "html"))))
  (is (string-equal "html"
                    (dom:node-name (first ($ "html")))))
  (is (dom:node-p (first ($ "#source"))))
  (is (= 2
         (length ($ ".fixed"))))
  (is (= 2
         (length ($ "article header, article blockquote"))))
  (is (eq (first ($ "ul"))
          (first ($ "#list")))))

(test fun-add
  (init-test-environment)
  (is (= (+ (length ($ "li")) (length ($ "p")))
         (length ($ "li" (add "p")))))
  (is (= (+ (length ($ "head")) (length ($ "body")))
         (length ($ "body" (add "head")))))
  (is (eq (first ($ "article"))
          (first (last ($ "li" (add "article")))))))

(test fun-add-class
  (init-test-environment)
  (is (string-equal "foo"
                    (dom:get-attribute (first ($ "article" (add-class "foo"))) "class")))
  (is (string-equal "foo bar"
                    (dom:get-attribute (first ($ "article" (add-class "bar"))) "class")))
  (is (string-equal (format NIL "~a ~a" (dom:get-attribute (first ($ "div")) "class") "baz")
                    (dom:get-attribute (first ($ "div" (add-class "baz"))) "class"))))
  
(test (fun-after :depends-on (AND . (fun-children)))
  (init-test-environment)
  (is (eq (first ($ "article blockquote p"))
          (first ($ "article blockquote p" (after "<div>Foo</div>")))))
  (is (= 2
         (length ($ "article blockquote" (children)))))
  (is (string-equal "div"
                    (dom:node-name (first (last ($ "article blockquote" (children))))))))

(test fun-ancestor
  (init-test-environment)
  (is (eq (first ($ "article"))
          (first ($ "article header, article blockquote" (ancestor)))))
  (is (eq (first ($ "#source"))
          (first ($ "article p, #source p" (ancestor))))))

(test (fun-append :depends-on (AND . (fun-children)))
  (init-test-environment)
  (is (eq (first ($ "#target"))
          (first ($ "#target" (append ($ "#source article"))))))
  (is (= 1
         (length ($ "#target" (children)))))
  (is (string-equal "article"
                    (dom:node-name (first ($ "#target article")))))
  (is (= 2
         (length ($ "#target article" (children))))))

(test (fun-append-to :depends-on (AND . (fun-children)))
  (init-test-environment)
  (is (eq (first ($ "#source article"))
          (first ($ "#source article" (append-to "#target")))))
  (is (= 1
         (length ($ "#target" (children)))))
  (is (string-equal "article"
                    (dom:node-name (first ($ "#target article")))))
  (is (= 2
         (length ($ "#target article" (children))))))

(test fun-attr
  (init-test-environment)
  (is (string-equal "list"
                    (first ($ "#source ul" (attr :id)))))
  (is (eq (first ($ "#source article"))
          (first ($ "#source article" (attr :foo "bar")))))
  (is (string-equal (dom:get-attribute (first ($ "#source article")) "foo")
                    (first ($ "#source article" (attr :foo))))))

(test (fun-before :depends-on (AND . (fun-children)))
  (init-test-environment)
  (is (eq (first ($ "article blockquote p"))
          (first ($ "article blockquote p" (before "<div>Foo</div>")))))
  (is (= 2
         (length ($ "article blockquote" (children)))))
  (is (string-equal "div"
                    (dom:node-name (first ($ "article blockquote" (children)))))))

(test fun-children
  (init-test-environment)
  (is (= 3
         (length ($ "ul" (children)))))
  (is (string-equal "p"
          (dom:node-name (first ($ "#source" (children))))))
  (is-false (> 3
               (length ($ "#source" (children))))))

(test fun-child-index
  (init-test-environment)
  (is (= 1
         (first ($ "h1" (child-index)))))
  (is (= 1
         (first ($ "#source>p" (child-index)))))
  (is (= 3
         (first ($ "#source>ul" (child-index))))))

(test (fun-clone :depends-on (AND . (fun-serialize)))
  (init-test-environment)
  (is-false (eq (first ($ "article"))
                (first ($ "article" (clone)))))
  (is (string-equal (first ($ "article" (serialize)))
                    (first ($ "article" (clone) (serialize))))))

(test fun-closest
  (init-test-environment)
  (is (eq (first ($ "div"))
          (first ($ "article p" (closest "#source"))))))

(test fun-contains
  (init-test-environment)
  (is (eq (first ($ "title"))
          (first ($ "body, #target, title" (contains "lQuery Test")))))
  (is (eq (first ($ "#target"))
          (first ($ "body, #target, title" (contains "")))))
  (is-false (eq (first ($ "body"))
                (first ($ "body, #target, title" (contains "Foo"))))))

(test fun-contents
  (init-test-environment)
  (is (= 7
         (length ($ "#list" (contents)))))
  (is (= 1
         (length ($ "title" (contents)))))
  (is-false (eq (first ($ "title"))
                (first ($ "head" (contents))))))

(test fun-css
  (init-test-environment)
  (is (eq (first ($ "#target"))
          (first ($ "#target" (css :color "blue")))))
  (is (string-equal "color:blue;"
                    (dom:get-attribute (first ($ "#target")) "style")))
  (is (string-equal "blue"
                    (first ($ "#target" (css :color)))))
  (is (string-equal "color:red;"
                    (dom:get-attribute (first ($ "#target" (css :color "red"))) "style")))
  (is (string-equal "color:red;foo:bar;"
                    (dom:get-attribute (first ($ "#target" (css :foo "bar"))) "style")))
  (is (string-equal "color:blue;"
                    (dom:get-attribute (first ($ "#target" (css :foo "" :color "blue"))) "style"))))

(test fun-data
  (init-test-environment)
  (is (eq (first ($ "#target"))
          (first ($ "#target" (data :foo "bar")))))
  (is (string-equal "bar"
                    (first ($ "#target" (data :foo))))))

(test fun-deepest 
  (init-test-environment)
  (is (eq (first ($ "#source>p"))
          (first ($ "#source" (deepest)))))
  (is (eq (first ($ "title"))
          (first ($ "html" (deepest))))))

(test (fun-detach :depends-on (AND . (fun-remove))))

(test fun-each
  (init-test-environment)
  (is (string-equal "AAAA"
                    (with-output-to-string (s)
                      ($ "li, article, #target" 
                         (each (lambda (node) (progn (format s "A")
                                                     (string-equal "li" (dom:node-name node)))))))))
  (is (eq (first ($ "li, article, #target"))
          (first ($ "li, article, #target" (each (lambda (node) (string-equal "li" (dom:node-name node))))))))
  (is (eq (first (last ($ "li, article, #target")))
          (first (last ($ "li, article, #target" (each (lambda (node) (string-equal "li" (dom:node-name node))))))))))

(test fun-empty
  (init-test-environment)
  (is (= 0
         (length (dom:child-nodes (first ($ "article" (empty))))))))

(test fun-eq
  (init-test-environment)
  (is (= 1
         (length ($ "li" (eq 1)))))
  (is (eq (second ($ "li"))
          (first ($ "li" (eq 1))))))

(test fun-even
  (init-test-environment)
  (is (= 1
         (length ($ "li" (even)))))
  (is (eq (second ($ "li"))
          (first ($ "li" (even))))))

(test fun-filter
  (init-test-environment)
  (is (eq (second ($ "head, body"))
          (first ($ "head, body" (filter (lambda (node) (string-equal (dom:node-name node) "body")))))))
  (is (eq (second ($ "head, body"))
          (first ($ "head, body" (filter "body"))))))

(test fun-find
  (init-test-environment)
  (is (= 3
         (length ($ "html" (find "li")))))
  (is (eq (first ($ "blockquote p"))
          (first ($ "html" (find "blockquote") (find "p"))))))

(test fun-first
  (init-test-environment)
  (is (= 1
         (length ($ "li" (first)))))
  (is (eq (first ($ "li"))
          (first ($ "li" (first))))))

(test fun-gt
  (init-test-environment)
  (is (= 2
         (length ($ "li" (gt 1)))))
  (is (eq (second ($ "li"))
          (first ($ "li" (gt 1))))))

(test fun-has
  (init-test-environment)
  (is (= 2
         (length ($ "head, body, article" (has "p")))))
  (is (eq (first ($ "body"))
          (first ($ "head, body, article" (has "p"))))))

(test fun-has-class
  (init-test-environment)
  (is (eq T ($ "head, body, div" (has-class "fixed"))))
  (is-false (eq T ($ "div" (has-class "foobar")))))

(test fun-hide
  (init-test-environment)
  (is (string-equal "display:none;"
                    (dom:get-attribute (first ($ "article" (hide))) "style"))))

(test (fun-html :depends-on (AND . (fun-serialize)))
  (init-test-environment)
  (is (eq (first ($ "li"))
          (first ($ "li" (html "FOO")))))
  (is (string-equal "FOO"
                    (first ($ "li" (html))))))

(test fun-index
  (init-test-environment)
  (is (= 2
         (first ($ "article" (index)))))
  (is (= 0
         (first ($ "article p" (index))))))

(test fun-initialize
  (lquery:initialize NIL)
  (is (eq (first ($ (initialize *test-file*)))
          (first ($)))))

(test (fun-insert-after :depends-on (AND . (fun-contains)))
  (init-test-environment)
  (is-false (eq (first ($ "article"))
                (first ($ "article" (insert-after "#target")))))
  (is (= 4
         (length ($ "body" (children)))))
  (is (string-equal "article"
                    (dom:node-name (first (last ($ "body" (children))))))))

(test (fun-insert-before :depends-on (AND . (fun-contains)))
  (init-test-environment)
  (is-false (eq (first ($ "article"))
                (first ($ "article" (insert-before "#target")))))
  (is (= 4
         (length ($ "body" (children)))))
  (is (string-equal "article"
                    (dom:node-name (nth 2 ($ "body" (children)))))))

(test fun-is
  (init-test-environment)
  (is (eq T ($ "head, body, div" (is "#source"))))
  (is-false (eq T ($ "head, body" (is "#source")))))

(test fun-is-empty
  (init-test-environment)
  (is (eq NIL ($ "title" (is-empty))))
  (is (eq T ($ "#target" (is-empty)))))

(test fun-last
  (init-test-environment)
  (is (= 1
         (length ($ "div" (last)))))
  (is (eq (first ($ "#target"))
          (first ($ "div" (last))))))

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
                    (first ($ "div" (map (lambda (node) (dom:get-attribute node "id"))))))))

(test fun-next
  (init-test-environment)
  (is (eq (first ($ "#list"))
          (first ($ "#source>p" (next)))))
  (is (eq (first ($ "article blockquote"))
          (first ($ "article header" (next)))))
  (is (eq NIL ($ "article header" (next "div")))))

(test (fun-next-all :depends-on (AND . (fun-first fun-last)))
  (init-test-environment)
  (is (= 2
         (length ($ "#list li" (first) (next-all)))))
  (is (eq (first ($ "#target"))
          (first ($ "h1" (next-all) (last)))))
  (is (eq (first ($ "#list"))
          (first ($ "#source p" (next-all "ul"))))))

(test fun-next-until
  (init-test-environment)
  (is (= 1
         (length ($ "h1" (next-until "#target")))))
  (is (eq (first ($ "#source"))
          (first (last ($ "h1" (next-until "#target")))))))

(test fun-node
  (init-test-environment)
  (is (eq (first ($ "li"))
          ($ "li" (node))))
  (is (eq (first ($ "#target"))
          ($ "div" (node 1)))))

(test fun-not
  (init-test-environment)
  (is (= 3
         (length ($ "head, body, li" (not "li")))))
  (is (eq (first ($ "li"))
          (first ($ "head, body, li" (not "li"))))))

(test fun-not-empty
  (init-test-environment)
  (is (= 1
         (length ($ "title" (not-empty)))))
  (is (eq NIL
          ($ "#target" (not-empty)))))

(test fun-odd
  (init-test-environment)
  (is (= 2
         (length ($ "li" (odd)))))
  (is (eq (first ($ "li"))
          (first ($ "li" (odd))))))

(test fun-parent
  (init-test-environment)
  (is (eq (first ($ "body"))
          (first ($ "h1" (parent)))))
  (is (eq NIL ($ "h1" (parent "#source")))))

(test fun-parents
  (init-test-environment)
  (is (eq (first ($ "html"))
          (first (last ($ "article p" (parents))))))
  (is (eq (first ($ "#source"))
          (first (last ($ "article p" (parents "div")))))))

(test fun-parents-until
  (init-test-environment)
  (is (eq (first ($ "#source"))
          (first (last ($ "article p" (parents-until "body")))))))

(test (fun-prepend :depends-on (AND . (fun-children)))
  (init-test-environment)
  (is (eq (first ($ "#list"))
          (first ($ "#list" (prepend ($ "article"))))))
  (is (= 4
         (length ($ "#list" (children)))))
  (is (string-equal "article"
                    (dom:node-name (first ($ "#list" (children)))))))

(test (fun-prepend-to :depends-on (AND . (fun-children)))
  (init-test-environment)
  (is (eq (first ($ "article"))
          (first ($ "article" (prepend-to ($ "#list"))))))
  (is (= 4
         (length ($ "#list" (children)))))
  (is (string-equal "article"
                    (dom:node-name (first ($ "#list" (children)))))))

(test fun-prev
  (init-test-environment)
  (is (eq (first ($ "#list"))
          (first ($ "article" (prev)))))
  (is (eq (first ($ "article header"))
          (first ($ "article blockquote" (prev)))))
  (is (eq NIL ($ "article blockquote" (prev "div")))))

(test (fun-prev-all :depends-on (AND . (fun-first fun-last)))
  (init-test-environment)
  (is (= 2
         (length ($ "#list li" (last) (prev-all)))))
  (is (eq (first ($ "h1"))
          (first ($ "#target" (prev-all) (last)))))
  (is (eq (first ($ "#list"))
          (first ($ "article" (prev-all "ul"))))))

(test fun-prev-until
  (init-test-environment)
  (is (= 1
         (length ($ "#target" (prev-until "h1")))))
  (is (eq (first ($ "#source"))
          (first (last ($ "#target" (prev-until "h1")))))))

(test (fun-remove :depends-on (AND . (fun-children)))
  (init-test-environment)
  (is (eq (first ($ "li"))
          (first ($ "li" (remove)))))
  (is (= 0
         (length ($ "#list" (children))))))

(test fun-remove-attr
  (init-test-environment)
  (is (eq (first ($ "#source"))
          (first ($ "#source" (remove-attr "class")))))
  (is (eq NIL (dom:has-attribute (first ($ "#source")) "class"))))

(test (fun-remove-class :depends-on (AND . (fun-has-class fun-add-class)))
  (init-test-environment)
  (is (eq (first ($ "#source"))
          (first ($ "#source" (remove-class "fixed")))))
  (is (eq NIL ($ "#source" (has-class "fixed"))))
  ($ "#source" (add-class "foo" "bar" "baz") (remove-class "bar"))
  (is (eq NIL ($ "#source" (has-class "bar"))))
  (is (eq T ($ "#source" (has-class "baz")))))

(test (fun-remove-data :depends-on (AND . (fun-data)))
  (init-test-environment)
  ($ "#source" (data :foo "bar"))
  (is (eq (first ($ "#source"))
          (first ($ "#source" (remove-data "foo")))))
  (is (eq NIL (dom:has-attribute (first ($ "#source")) "data-foo"))))

(test (fun-replace-all :depends-on (AND . (fun-children)))
  (init-test-environment)
  (is (eq (first ($ "article"))
          (first ($ "article" (replace-all "#target")))))
  (is (= 3
         (length ($ "body" (children)))))
  (is (string-equal "article"
                    (dom:node-name (first (last ($ "body" (children))))))))

(test (fun-replace-with :depends-on (AND . (fun-children)))
  (init-test-environment)
  (is (eq (first ($ "#target"))
          (first ($ "#target" (replace-with ($ "article"))))))
  (is (= 3
         (length ($ "body" (children)))))
  (is (string-equal "article"
                    (dom:node-name (first (last ($ "body" (children))))))))

(test fun-show
  (init-test-environment)
  (is (string-equal "display:block;"
                    (dom:get-attribute (first ($ "article" (show))) "style"))))

(test fun-siblings
  (init-test-environment)
  (is (= 2
         (length ($ "#source" (siblings)))))
  (is (eq (first ($ "#target"))
          (second ($ "#source" (siblings))))))

(test fun-size
  (init-test-environment)
  (is (= (length ($ "li"))
         ($ "li" (size)))))

(test fun-slice
  (init-test-environment)
  (is (= 3
         (length ($ "li, p, div" (slice 1 4)))))
  (is (eq (first ($ "#source>p"))
          (first ($ "li, p, div" (slice 1 4)))))
  (is (eq (first ($ "#list li"))
          (second ($ "li, p, div" (slice 1 4))))))

(test fun-text
  (init-test-environment)
  (is (eq (first ($ "#source>p"))
          (first ($ "#source>p" (text "FooBar")))))
  (is (string-equal "FooBar"
                    (first ($ "#source>p" (text))))))

(test (fun-toggle-class :depends-on (AND . (fun-has-class)))
  (init-test-environment)
  (is-false (eq T ($ "article" (has-class "foo"))))
  (is (eq (first ($ "article"))
          (first ($ "article" (toggle-class "foo")))))
  (is (eq T ($ "article" (has-class "foo"))))
  (is (eq (first ($ "article"))
          (first ($ "article" (toggle-class "foo")))))
  (is-false (eq T ($ "article" (has-class "foo")))))

(test fun-unwrap
  (init-test-environment)
  (is (eq (first ($ "li"))
          (first ($ "li" (unwrap)))))
  (is (eq NIL ($ "#list")))
  (is (eq (first ($ "#source"))
          (dom:parent-node (first ($ "li"))))))

(test fun-val
  (init-test-environment)
  (is (eq (first ($ "article"))
          (first ($ "article" (val "foo")))))
  (is (string-equal "foo"
                    (first ($ "article" (val))))))

(test fun-wrap
  (init-test-environment)
  (is (eq (first ($ "li"))
          (first ($ "li" (wrap "<div></div>")))))
  (is-false (eq (first ($ "#list"))
                (dom:parent-node (first ($ "li")))))
  (is-false (eq (dom:parent-node (first ($ "li")))
                (dom:parent-node (second ($ "li"))))))

(test fun-wrap-all
  (init-test-environment)
  (is (eq (first ($ "li"))
          (first ($ "li" (wrap-all "<div></div>")))))
  (is-false (eq (first ($ "#list"))
                (dom:parent-node (first ($ "li")))))
  (is (eq (dom:parent-node (first ($ "li")))
          (dom:parent-node (second ($ "li"))))))

(test fun-wrap-inner
  (init-test-environment)
  (is (eq (first ($ "#list"))
          (first ($ "#list" (wrap-inner "<div></div>")))))
  (is-false (eq (first ($ "#list"))
                (dom:parent-node (first ($ "li")))))
  (is (eq (dom:parent-node (first ($ "li")))
          (dom:parent-node (second ($ "li"))))))

(test fun-write-to-file
  (init-test-environment)
  (is (eq (first ($ "article"))
          (first ($ "article" (write-to-file (merge-pathnames "test-output.html" (asdf:system-source-directory :lquery)))))))
  (is (probe-file (merge-pathnames "test-output.html" (asdf:system-source-directory :lquery))))
  (is (delete-file (merge-pathnames "test-output.html" (asdf:system-source-directory :lquery)))))

(test fun-serialize
  (init-test-environment)
  (is (string-equal "<p>Article Text</p>"
                    (first ($ "article p" (serialize))))))
