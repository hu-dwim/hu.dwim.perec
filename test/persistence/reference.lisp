;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

(defsuite* (test/persistence/reference :in test/persistence))

(defpclass* referred-test ()
  ())

(defpclass* reference-test ()
  ((referred :type referred-test)))

(defpclass* reference-or-unbound-test ()
  ((referred :type (or unbound referred-test))))

(defpclass* reference-or-null-test ()
  ((referred :type (or null referred-test))))

(defpclass* reference-or-unbound-null-test ()
  ((referred :type (or unbound null referred-test))))

(defmacro with-reference-or-null-transaction (&body body)
  `(with-transaction
    (let ((referred (make-instance 'referred-test))
          (reference (make-instance 'reference-or-null-test)))
      (declare (ignorable referred reference))
      ,@body)))

(deftest test/persistence/reference/initial-value/1 ()
  (with-and-without-caching-slot-values
    (with-one-and-two-transactions (make-instance 'reference-or-null-test)
      (is (eq nil (referred-of -object-))))))

(deftest test/persistence/reference/initial-value/2 ()
  (with-and-without-caching-slot-values
    (with-one-and-two-transactions (make-instance 'reference-or-unbound-test)
      (is (not (slot-boundp -object- 'referred))))))

(deftest test/persistence/reference/initial-value/3 ()
  (with-and-without-caching-slot-values
    (with-one-and-two-transactions (make-instance 'reference-or-unbound-test)
      (is (not (slot-boundp -object- 'referred))))))

(deftest test/persistence/reference/store-value/1 ()
  (with-and-without-caching-slot-values
    (with-reference-or-null-transaction
      (setf (referred-of reference) referred)
      (is (eq referred (referred-of reference))))))
