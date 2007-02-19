(in-package :cl-perec-test)

(defsuite* (test/query/purge :in test/query))

(defun check-existing-records (&rest content)
  (iter (for (table-id expected-records) on content by 'cddr)
        (with table-name = (format nil "_purge_~d_test" table-id))
        (with column-name = (make-symbol (format nil "_int_attr_~d" table-id)))
        (with records-in-database = (apply 'nconc (execute (sql `(select (,column-name) (,table-name))))))
        (is (sort records-in-database #'<=) expected-records)))

(defmacro run-purge-test (&body body)
  `(progn
    (fill-data-7)
    (with-transaction* (:default-terminal-action :rollback)
      (when *show-query*
        (format t "~{~&~A~}" ',body))
      ,@body)))

(defmacro def-purge-query-test (name class-id attr-value &body expected)
  (bind ((class (read-from-string (format nil "purge-~d-test" class-id)))
         (accessor (read-from-string (format nil "int-attr-~d-of" class-id))))
    `(deftest ,name ()
     (run-purge-test
       (select ((o ,class))
         ,@(when attr-value (list `(assert (= (,accessor o) ,attr-value))))
         (purge o))
       (apply 'check-existing-records ,(first expected))))))

;;;
;;;   0       1
;;;          / \
;;;         2   3
;;;          \ / \ 
;;;           4   7
;;;          / \
;;;         5   6

(defpclass* purge-0-test ()
  ((int-attr-0 :type integer-32)))

(defpclass* purge-1-test ()
  ((int-attr-1 :type integer-32)))

(defpclass* purge-2-test (purge-1-test)
  ((int-attr-2 :type integer-32)))

(defpclass* purge-3-test (purge-1-test)
  ((int-attr-3 :type integer-32)))

(defpclass* purge-4-test (purge-2-test purge-3-test)
  ()
  (:abstract #t))

(defpclass* purge-5-test (purge-4-test)
  ((int-attr-5 :type integer-32)))

(defpclass* purge-6-test (purge-4-test)
  ((int-attr-6 :type integer-32)))

(defpclass* purge-7-test (purge-3-test)
  ((int-attr-7 :type integer-32)))

(defixture fill-data-7
  (with-transaction
    (export-all-classes)
    (purge-objects 'purge-1-test)
    (purge-objects 'purge-2-test)
    (purge-objects 'purge-3-test)
    (purge-objects 'purge-4-test)
    (purge-objects 'purge-5-test)
    (purge-objects 'purge-6-test)
    (purge-objects 'purge-7-test)
    (purge-objects 'purge-8-test)

    (make-instance 'purge-0-test :int-attr-0 0)
    (make-instance 'purge-0-test :int-attr-0 1)
    (make-instance 'purge-1-test :int-attr-1 0)
    (make-instance 'purge-1-test :int-attr-1 1)
    (make-instance 'purge-2-test :int-attr-1 2 :int-attr-2 0)
    (make-instance 'purge-2-test :int-attr-1 3 :int-attr-2 1)
    (make-instance 'purge-3-test :int-attr-1 4 :int-attr-3 0)
    (make-instance 'purge-3-test :int-attr-1 5 :int-attr-3 1)
    (make-instance 'purge-5-test :int-attr-1 6 :int-attr-2 2 :int-attr-3 2 :int-attr-5 0)
    (make-instance 'purge-5-test :int-attr-1 7 :int-attr-2 3 :int-attr-3 3 :int-attr-5 1)
    (make-instance 'purge-6-test :int-attr-1 8 :int-attr-2 4 :int-attr-3 4 :int-attr-6 0)
    (make-instance 'purge-6-test :int-attr-1 9 :int-attr-2 5 :int-attr-3 5 :int-attr-6 1)
    (make-instance 'purge-7-test :int-attr-1 10 :int-attr-3 6 :int-attr-7 0)
    (make-instance 'purge-7-test :int-attr-1 11 :int-attr-3 7 :int-attr-7 1)))

(def-purge-query-test test/query/purge/delete-0-all 0 nil
  `(0 nil
    1 (0 1 2 3 4 5 6 7 8 9 10 11)
    2 (0 1 2 3 4 5)
    3 (0 1 2 3 4 5 6 7)
    5 (0 1)
    6 (0 1)
    7 (0 1)))

(def-purge-query-test test/query/purge/delete-0-one 0 0
  `(0 (1)
    1 (0 1 2 3 4 5 6 7 8 9 10 11)
    2 (0 1 2 3 4 5)
    3 (0 1 2 3 4 5 6 7)
    5 (0 1)
    6 (0 1)
    7 (0 1)))

(def-purge-query-test test/query/purge/delete-1-all 1 nil
  '(0 (0 1) 1 nil 2 nil 3 nil 5 nil 6 nil 7 nil))

(def-purge-query-test test/query/purge-delete-1-one 1 0
  '(0 (0 1)
    1 (1 2 3 4 5 6 7 8 9 10 11)
    2 (1 2 3 4 5)
    3 (1 2 3 4 5 6 7)
    5 (1)
    6 (1)
    7 (1)))

(def-purge-query-test test/query/purge/delete-2-all 2 nil
  '(0 (0 1)
    1 (0 1 4 5 10 11)
    2 nil
    3 (0 1 6 7)
    5 nil
    6 nil
    7 (0 1)))

(def-purge-query-test test/query/purge/delete-2-one 2 0
  `(0 (0 1)
    1 (0 1 3 4 5 6 7 8 9 10 11)
    2 (1 3 5)
    3 (0 1 3 5 6 7)
    5 (1)
    6 (1)
    7 (0 1)))

(def-purge-query-test test/query/purge/delete-3-one 3 0
  `(0 (0 1)
    1 (0 1 2 3 5 6 7 8 9 10 11)
    2 (0 1 3 5)
    3 (1 3 5 7)
    5 (1)
    6 (1)
    7 (1)))

(def-purge-query-test test/query/purge/delete-4-one 4 0
  `(0 (0 1)
    1 (0 1 2 3 4 5 7 9 10 11)
    2 (0 1 3 5)
    3 (0 1 3 5 6 7)
    5 (1)
    6 (1)
    7 (0 1)))

(def-purge-query-test test/query/purge/delete-5-one 5 0
  `(0 (0 1)
    1 (0 1 2 3 4 5 7 8 9 10 11)
    2 (0 1 3 4 5)
    3 (0 1 3 4 5 6 7)
    5 (1)
    6 (0 1)
    7 (0 1)))

(def-purge-query-test test/query/purge/delete-6-one 6 0
  `(0 (0 1)
    1 (0 1 2 3 4 5 6 7 9 10 11)
    2 (0 1 2 3 5)
    3 (0 1 2 3 5 6 7)
    5 (0 1)
    6 (1)
    7 (0 1)))

(def-purge-query-test test/query/purge/delete-7-one 7 0
  `(0 (0 1)
    1 (0 1 2 3 4 5 6 7 8 9 11)
    2 (0 1 2 3 4 5)
    3 (0 1 2 3 4 5 7)
    5 (0 1)
    6 (0 1)
    7 (1)))
