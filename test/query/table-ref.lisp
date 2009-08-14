;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(defsuite* (test/query/table-ref :in test/query))

(defpclass* abstract-test ()
  ()
  (:abstract #t))

(defpclass* super-1-test ()
  ())

(defpclass* super-2-test ()
  ())

(defpclass* sub-1-test (super-1-test super-2-test)
  ())

(defpclass* sub-2-test (super-1-test super-2-test)
  ())

(defixture create-type-test-data
  (with-transaction
    (purge-instances 'super-1-test)
    (purge-instances 'super-2-test)
    (purge-instances 'sub-1-test)
    (purge-instances 'sub-2-test)
    (make-instance 'sub-1-test)
    (make-instance 'sub-2-test)))

(defmacro type-test (&body body)
  `(progn
    (create-type-test-data)
    (finishes
      (run-queries
        ,@body))))

(deftest test/query/table-ref/none ()
  (type-test
    (select (o)
      (from o))))

(deftest test/query/table-ref/and-self ()
  (type-test
    (select (o)
      (from o)
      (where (and
              (typep o 'super-1-test)
              (typep o 'super-1-test))))))

(deftest test/query/table-ref/and-supers ()
  (type-test
    (select (o)
      (from o)
      (where (and
              (typep o 'super-1-test)
              (typep o 'super-2-test))))))

(deftest test/query/table-ref/or-supers ()
  (type-test
    (select (o)
      (from o)
      (where (typep o '(or super-1-test super-2-test))))))

(deftest test/query/table-ref/abstract ()
  (type-test
   (select (o)
     (from (o abstract-test)))))
