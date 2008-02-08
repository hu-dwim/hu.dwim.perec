(in-package :cl-perec-test)

(defsuite* (test/persistence/table :in test/persistence))

(deftest test/persistence/table/persistent-object ()
  (is (not (primary-table-of (find-class 'persistent-object)))))

(defpclass table-t1-test ()
  ((name :type string)))
   
(defpclass table-t2-test (table-t1-test)
  ((name)))

(deftest test/persistence/table/inheritance ()
  (is (not (null (find '_name (columns-of (primary-table-of (find-class 'table-t1-test))) :key #'rdbms::name-of))))
  (is (null (find '_name (columns-of (primary-table-of (find-class 'table-t2-test))) :key #'rdbms::name-of))))

(defpclass table-a1-test ()
  ()
  (:abstract #t))
   
(defpclass table-b1-test (table-a1-test)
  ())

(defpclass table-c1-test (table-a1-test)
  ())

(defpclass table-a2-test ()
  ()
  (:abstract #t))

(defpclass table-b2-test (table-a2-test)
  ())

(defpclass table-c2-test (table-a2-test)
  ())

(defpclass table-d2-test (table-b2-test table-c2-test)
  ())

(deftest test/persistence/table/primary-table ()
  (bind ((a1 (find-class 'table-a1-test))
         (b1 (find-class 'table-b1-test))
         (c1 (find-class 'table-c1-test))
         (a2 (find-class 'table-a2-test))
         (b2 (find-class 'table-b2-test))
         (c2 (find-class 'table-c2-test))
         (d2 (find-class 'table-d2-test)))
    (mapc #'finalize-inheritance (list a1 b1 c1 a2 b2 c2 d2))

    ;; checks for a1
    (is (null (primary-table-of a1)))
    (is (null (data-tables-of a1)))
    (is (equal (list :append (primary-table-of c1) (primary-table-of b1))
               (primary-tables-of a1)))

    ;; checks for b1
    (is (not (null (primary-table-of b1))))
    (is (equal (list (primary-table-of b1))
               (data-tables-of b1)))
    (is (equal (list :append (primary-table-of b1))
               (primary-tables-of b1)))
 
    ;; checks for a2
    (is (null (primary-table-of a2)))
    (is (null (data-tables-of a2)))
    (is (equal (list :union (primary-table-of c2) (primary-table-of b2))
               (primary-tables-of a2)))

    ;; checks for b2
    (is (not (null (primary-table-of b2))))
    (is (equal (list (primary-table-of b2))
               (data-tables-of b2)))
    (is (equal (list :append (primary-table-of b2))
               (primary-tables-of b2)))

    ;; checks for d2
    (is (not (null (primary-table-of d2))))
    (is (equal (list (primary-table-of d2) (primary-table-of b2) (primary-table-of c2))
               (data-tables-of d2)))
    (is (equal (list :append (primary-table-of d2))
               (primary-tables-of d2)))))
