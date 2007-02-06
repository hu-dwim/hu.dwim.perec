(in-package :cl-perec-test)

(defsuite* (test/query/purge :in test/query))

(defun check-existing-records (table-id expected)
  (bind ((table (format nil "_purge_~d_test" table-id))
         (column (make-symbol (format nil "_int_attr_~d" table-id))))
    (is (equal (sort (apply 'nconc (execute (sql `(select (,column) (,table))))) #'<=)
               expected))))

(defun int-list (from below)
  (iter (for i from from below below)
        (collect i)))

(defmacro run-purge-test (&body body)
  `(progn
    (fill-data-7)
    (with-transaction* (:default-terminal-action :rollback)
      (when *show-query*
        (format t "~{~&~A~}" ',body))
      ,@body)))

(defpclass* purge-1-test ()
  ((int-attr-1 :type integer-32)))

(defpclass* purge-2-test (purge-1-test)
  ((int-attr-2 :type integer-32)))

(defpclass* purge-3-test ()
  ((int-attr-3 :type integer-32)))

(defixture fill-data-7
  (with-transaction
    (export-all-classes)
    (purge-objects 'purge-1-test)
    (purge-objects 'purge-2-test)
    (purge-objects 'purge-3-test)
    (iter (for i from 0 below 5)
          (make-instance 'purge-1-test :int-attr-1 i)
          (make-instance 'purge-3-test :int-attr-3 i))
    (iter (for i from 5 below 10)
          (make-instance 'purge-2-test :int-attr-1 i :int-attr-2 i))))

(deftest test/query/purge/simple/all ()
  (run-purge-test
    (select ((o purge-3-test))
      (purge o))
    (check-existing-records 3 nil)))

(deftest test/query/purge/simple/one ()
  (run-purge-test
    (select ((o purge-3-test))
      (assert (= (int-attr-3-of o) 0))
      (purge o))
    (check-existing-records 3 (int-list 1 5))))
   
(deftest test/query/purge/polymorph/all ()
  (run-purge-test
    (select ((o purge-1-test))
      (purge o))
    (check-existing-records 1 nil)
    (check-existing-records 2 nil)))

(deftest test/query/purge/polymorph/one ()
  (run-purge-test
    (select ((o purge-1-test))
      (assert (= (int-attr-1-of o) 0))
      (purge o))
    (check-existing-records 1 (int-list 1 10))
    (check-existing-records 2 (int-list 5 10))))

(deftest test/query/purge/polymorph/sub ()
  (run-purge-test
    (select ((o purge-2-test))
      (purge o))
    (check-existing-records 1 (int-list 0 5))
    (check-existing-records 2 nil)))

(deftest test/query/purge/polymorph/super ()
  (run-purge-test
    (select ((o purge-1-test))
      (assert (= (int-attr-1-of o) 9))
      (purge o))
    (check-existing-records 1 (int-list 0 9))
    (check-existing-records 2 (int-list 5 9))))
