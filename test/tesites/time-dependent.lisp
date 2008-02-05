;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

;;;;;;;;;;;;;;;;;;
;;; Time dependent

(defsuite* (test/tesites/time-dependent :in test/tesites))

(defpclass* time-dependent-test ()
  ((population :type integer-32 :time-dependent #t)))

(deftest test/tesites/time-dependent/validity-not-specified ()
  (with-transaction
    (signals unbound-slot-t (population-of (make-instance 'time-dependent-test)))))

(deftest test/tesites/time-dependent/unbound/1 ()
  (with-transaction
    (with-validity "2007-01-01"
      (signals unbound-slot-t (population-of (make-instance 'time-dependent-test))))))

(deftest test/tesites/time-dependent/unbound/2 ()
  (with-transaction
    (bind ((instance (make-instance 'time-dependent-test)))
      (with-validity-range +beginning-of-time+ "2006-12-31"
        (setf (population-of instance) 1000))
      (with-validity-range "2008-01-01" +end-of-time+
        (setf (population-of instance) 2000))
      (with-validity-range "2007-01-01" "2007-12-31"
        (signals unbound-slot-t (population-of instance)))
      (with-validity-range "2006-01-01" "2007-12-31"
        (signals unbound-slot-t (population-of instance)))
      (with-validity-range "2007-01-01" "2008-12-31"
        (signals unbound-slot-t (population-of instance))))))

(deftest test/tesites/time-dependent/store-value/1 ()
  (with-validity "2007-01-01"
    (with-one-and-two-transactions
        (bind ((instance (make-instance 'time-dependent-test)))
          (setf (population-of instance) 1000)
          instance)
      (is (= 1000 (population-of -instance-))))))

(deftest test/tesites/time-dependent/store-value/2 ()
  (with-validity "2007-01-01"
    (with-one-and-two-transactions
        (bind ((instance (make-instance 'time-dependent-test)))
          (setf (population-of instance) 1000)
          (is (= (update-counter-of (command-counter-of *transaction*)) 1))
          (is (= (insert-counter-of (command-counter-of *transaction*)) (+ 1 3)))
          instance)
      (let ((update-counter (update-counter-of (command-counter-of *transaction*)))
            (insert-counter (insert-counter-of (command-counter-of *transaction*))))
        (setf (population-of -instance-) 1000)
        (is (= (update-counter-of (command-counter-of *transaction*)) (1+ update-counter)))
        (is (= (insert-counter-of (command-counter-of *transaction*)) insert-counter))))))

(deftest test/tesites/time-dependent/store-value/3 ()
  (with-one-and-two-transactions
      (bind ((instance (make-instance 'time-dependent-test)))
        (with-validity "2007"
          (setf (population-of instance) 1000))
        instance)
    (signals error
      (with-validity "2007-07"
        (setf (population-of -instance-) 1000)))))

(deftest test/tesites/time-dependent/values-having-validity/1 ()
  (with-one-and-two-transactions
      (bind ((instance (make-instance 'time-dependent-test)))
        (with-validity "2006"
          (setf (population-of instance) 2006))
        instance)
    (with-validity "2007"
      (setf (population-of -instance-) 2007))
    (with-validity-range "2006" "2007"
      (iter (for index :from 0)
            (for (value validity-start validity-end) :in-values-having-validity (population-of -instance-))
            (ecase index
              (0 (is (= 2006 value))
                 (is (local-time= (parse-datestring "2006-01-01") validity-start))
                 (is (local-time= (parse-datestring "2007-01-01") validity-end)))
              (1 (is (= 2007 value))
                 (is (local-time= (parse-datestring "2007-01-01") validity-start))
                 (is (local-time= (parse-datestring "2008-01-01") validity-end))))))))

