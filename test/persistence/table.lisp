(in-package :cl-perec-test)

(defsuite* test/table :in test/persistence)

(deftest test/table/persistent-object ()
  (is (not (prc::primary-table-of (find-class 'persistent-object)))))

(defpclass table-t1-test ()
  ((name :type string)))
   
(defpclass table-t2-test (table-t1-test)
  (name))

(deftest test/table/inheritance ()
  (is (not (null (find '_name (prc::columns-of (prc::primary-table-of (find-class 'table-t1-test))) :key #'rdbms::name-of))))
  (is (null (find '_name (prc::columns-of (prc::primary-table-of (find-class 'table-t2-test))) :key #'rdbms::name-of))))

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

(deftest test/table/primary-table ()
  (bind ((a1 (find-class 'table-a1-test))
         (b1 (find-class 'table-b1-test))
         (c1 (find-class 'table-c1-test))
         (a2 (find-class 'table-a2-test))
         (b2 (find-class 'table-b2-test))
         (c2 (find-class 'table-c2-test))
         (d2 (find-class 'table-d2-test)))
    (mapc #'finalize-inheritance (list a1 b1 c1 a2 b2 c2 d2))

    ;; checks for a1
    (is (null (prc::primary-table-of a1)))
    (is (null (prc::data-tables-of a1)))
    (is (equal (list 'append (prc::primary-table-of c1) (prc::primary-table-of b1))
               (prc::primary-tables-of a1)))

    ;; checks for b1
    (is (not (null (prc::primary-table-of b1))))
    (is (equal (list (prc::primary-table-of b1))
               (prc::data-tables-of b1)))
    (is (equal (list 'append (prc::primary-table-of b1))
               (prc::primary-tables-of b1)))
 
    ;; checks for a2
    (is (null (prc::primary-table-of a2)))
    (is (null (prc::data-tables-of a2)))
    (is (equal (list 'union (prc::primary-table-of c2) (prc::primary-table-of b2))
               (prc::primary-tables-of a2)))

    ;; checks for b2
    (is (not (null (prc::primary-table-of b2))))
    (is (equal (list (prc::primary-table-of b2))
               (prc::data-tables-of b2)))
    (is (equal (list 'append (prc::primary-table-of b2))
               (prc::primary-tables-of b2)))

    ;; checks for d2
    (is (not (null (prc::primary-table-of d2))))
    (is (equal (list (prc::primary-table-of d2) (prc::primary-table-of b2) (prc::primary-table-of c2))
               (prc::data-tables-of d2)))
    (is (equal (list 'append (prc::primary-table-of d2))
               (prc::primary-tables-of d2)))))
