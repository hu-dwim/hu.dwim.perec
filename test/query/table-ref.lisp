(in-package :cl-perec-test)

(defsuite* (test/query/table-ref :in test/query))

(defpclass* super-1-test ()
  ((attr1 :type integer)))

(defpclass* super-2-test ()
  ((attr1 :type integer)))

(defpclass* sub-1-test (super-1-test super-2-test)
  ((attr2 :type integer)))

(defpclass* sub-2-test (super-1-test super-2-test)
  ((attr2 :type integer)))

(defixture create-type-test-data
  (with-transaction
    (export-all-classes)
    (purge-objects 'super-1-test)
    (purge-objects 'super-2-test)
    (purge-objects 'sub-1-test)
    (purge-objects 'sub-2-test)
    (make-instance 'sub-1-test :attr1 1 :attr2 1)
    (make-instance 'sub-2-test :attr1 2 :attr2 2)))

(defmacro type-test (&body body)
  `(progn
    (create-type-test-data)
    (finishes
      (run-queries
        ,@body))))
        
(deftest test/query/table-ref/none ()
  (type-test
    (select (o)
      (collect o))))

(deftest test/query/table-ref/and-self ()
  (type-test
    (select (o)
      (assert (typep o 'super-1-test))
      (assert (typep o 'super-1-test))
      (collect o))))

(deftest test/query/table-ref/and-supers ()
  (type-test
    (select (o)
      (assert (typep o 'super-1-test))
      (assert (typep o 'super-2-test))
      (collect o))))

(deftest test/query/table-ref/or-supers ()
  (type-test
    (select (o)
      (assert (typep o '(or super-1-test super-2-test)))
      (collect o))))
