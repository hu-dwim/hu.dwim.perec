;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Temporal and time dependent

(defvar *temporal-time-dependent-class-name*)

(defsuite* (test/tesites/temporal-time-dependent :in test/tesites))

(defpclass* temporal-time-dependent-unbound-test ()
  ((population :type (or unbound integer-32) :time-dependent #t :temporal #t)))

(defpclass* temporal-time-dependent-null-test ()
  ((population :type (or null integer-32) :time-dependent #t :temporal #t)))

(defmacro with-temporal-time-dependent-test-classes (&body forms)
  (with-unique-names (body)
    `(flet ((,body ()
              ,@forms))
       (bind ((*temporal-time-dependent-class-name* 'temporal-time-dependent-unbound-test))
         (,body))
       (bind ((*temporal-time-dependent-class-name* 'temporal-time-dependent-null-test))
         (,body)))))

(deftest test/tesites/temporal-time-dependent/table ()
  (with-temporal-time-dependent-test-classes
    (ensure-finalized (find-class *temporal-time-dependent-class-name*))
    (is (null (columns-of (find-slot *temporal-time-dependent-class-name* 'population))))))

(deftest test/tesites/temporal-time-dependent/initial-value/unbound ()
  (with-transaction
    (signals unbound-slot-t (population-of (make-instance 'temporal-time-dependent-unbound-test)))))

(deftest test/tesites/temporal-time-dependent/initial-value/null ()
  (with-transaction
    (is (null (single-values-having-validity-value
               (population-of (make-instance 'temporal-time-dependent-null-test)))))))

(deftest test/tesites/temporal-time-dependent/initial-value/integer ()
  (with-transaction
    (with-temporal-time-dependent-test-classes
      (is (= 1000
             (single-values-having-validity-value
              (population-of (make-instance *temporal-time-dependent-class-name* :population 1000))))))))

(deftest test/tesites/temporal-time-dependent/store-value/1 ()
  (with-temporal-time-dependent-test-classes
    (with-validity "2007-01-01"
      (with-one-and-two-transactions
          (bind ((instance (make-instance *temporal-time-dependent-class-name*)))
            (setf (population-of instance) 1000)
            instance)
        (is (= 1000 (single-values-having-validity-value (population-of -instance-))))
        (is (= 1 (length (h-objects-of -instance-))))))))
