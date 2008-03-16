;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

;;;;;;;;;;;;
;;; Temporal

(defvar *temporal-class-name*)

(defsuite* (test/tesites/temporal :in test/tesites))

(defpclass* temporal-unbound-test ()
  ((population :type (or unbound integer-32) :temporal #t)))

(defpclass* temporal-null-test ()
  ((population :type (or null integer-32) :temporal #t)))

(defmacro with-temporal-test-classes (&body forms)
  (with-unique-names (body)
    `(flet ((,body ()
              ,@forms))
       (bind ((*temporal-class-name* 'temporal-unbound-test))
         (,body))
       (bind ((*temporal-class-name* 'temporal-null-test))
         (,body)))))

(deftest test/tesites/temporal/table ()
  (with-temporal-test-classes
    (ensure-finalized (find-class *temporal-class-name*))
    (is (null (columns-of (find-slot *temporal-class-name* 'population))))))

(deftest test/tesites/temporal/t-not-specified ()
  (with-transaction
    (with-temporal-test-classes
      (signals unbound-variable (make-instance *temporal-class-name*)))))

(deftest test/tesites/temporal/initial-value/unbound ()
  (with-transaction
    (with-default-t
      (signals unbound-slot-t (population-of (make-instance 'temporal-unbound-test))))))

(deftest test/tesites/temporal/initial-value/null ()
  (with-transaction
    (with-default-t
      (is (null (population-of (make-instance 'temporal-null-test))))
      (is (null (population-of (make-instance 'temporal-null-test :population nil)))))))

(deftest test/tesites/temporal/initial-value/integer ()
  (with-transaction
    (with-default-t
      (with-temporal-test-classes
        (is (= 1000 (population-of (make-instance *temporal-class-name* :population 1000))))))))

(deftest test/tesites/temporal/store-value/1 ()
  (with-temporal-test-classes
    (with-one-and-two-transactions
        (with-default-t
          (let ((instance (make-instance *temporal-class-name*)))
            (setf (population-of instance) 1000)
            instance))
      (with-default-t
        (is (= 1000 (population-of -instance-)))
        (is (= 1 (length (h-objects-of -instance-))))))))

(deftest test/tesites/temporal/store-value/2 ()
  (with-temporal-test-classes
    (bind (((:values instance t-value)
            (with-transaction
              (with-default-t
                (values (make-instance *temporal-class-name* :population 1000) *t*)))))
      (with-transaction
        (with-default-t
          (with-revived-instance instance
            (setf (population-of instance) 2000))))
      (with-transaction
        (with-default-t
          (with-revived-instance instance
            (is (= 2000 (population-of instance))))))
      (with-transaction
        (with-t t-value
          (with-revived-instance instance
            (is (= 1000 (population-of instance)))
            (is (= 2 (length (h-objects-of instance))))))))))

(defpclass* temporal-complex-test ()
  ((slot :type (or null integer-32))
   (slot-1 :type (or null integer-32) :temporal #t)
   (slot-2 :type (or null integer-32) :temporal #t)))

(deftest test/tesites/temporal/complex/same-t ()
  (with-one-and-two-transactions
      (with-default-t
        (bind ((instance
                (make-instance 'temporal-complex-test :slot 0 :slot-1 1000)))
          (setf (slot-2-of instance) 2000)
          instance))
    (with-default-t
      (is (= 0 (slot-of -instance-)))
      (is (= 1000 (slot-1-of -instance-)))
      (is (= 2000 (slot-2-of -instance-)))
      (is (= 1 (length (h-objects-of -instance-)))))))

(deftest test/tesites/temporal/complex/different-t ()
  (bind ((instance
          (with-transaction
            (with-default-t
              (make-instance 'temporal-complex-test :slot 0 :slot-1 1000)))))
    (with-transaction
      (with-default-t
        (with-revived-instance instance
          (is (= 0 (slot-of instance)))
          (is (= 1000 (slot-1-of instance)))
          (is (null (slot-2-of instance)))
          (setf (slot-2-of instance) 2000)
          (is (= 2 (length (h-objects-of instance)))))))
    (with-transaction
      (with-default-t
        (with-revived-instance instance
          (is (= 0 (slot-of instance)))
          (is (= 1000 (slot-1-of instance)))
          (is (= 2000 (slot-2-of instance))))))))
