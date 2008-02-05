;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

;;;;;
;;; T

(defsuite* (test/tesites/t :in test/tesites))

(defpclass* t-test ()
  ((population :type integer-32 :time-dependent #t :temporal #t)))

(defsuite* (test/tesites/temporal-time-dependent :in test/tesites))

(defpclass* t-a-test ()
  ((a-value :type integer-32 :time-dependent #t)))

(defpclass* t-b-test (t-a-test)
  ((b-value :type integer-32 :temporal #t :time-dependent #t)))

(deftest test/tesites/t/t-not-specified ()
  (with-transaction
    (with-validity "2007"
      (signals unbound-variable (population-of (make-instance 't-test))))))

(deftest test/tesites/t/validity-not-specified ()
  (with-transaction
    (with-default-t
      (signals unbound-slot-t (population-of (make-instance 't-test))))))

(deftest test/tesites/t/unbound/1 ()
  (with-transaction
    (with-default-t
      (with-validity "2007"
        (signals unbound-slot-t (population-of (make-instance 't-test)))))))

(deftest test/tesites/t/store-value/1 ()
  (with-validity "2007-01-01"
    (with-one-and-two-transactions
        (with-default-t
          (bind ((instance (make-instance 't-b-test)))
            (setf (b-value-of instance) 1000)
            instance))
      (with-default-t
        (is (= 1000 (b-value-of -instance-)))))))


