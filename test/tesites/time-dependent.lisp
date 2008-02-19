;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

;;;;;;;;;;;;;;;;;;
;;; Time dependent

(defvar *time-dependent-class-name*)

(defsuite* (test/tesites/time-dependent :in test/tesites))

(defpclass* time-dependent-unbound-test ()
  ((population :type (or unbound integer-32) :time-dependent #t)))

(defpclass* time-dependent-null-test ()
  ((population :type (or null integer-32) :time-dependent #t)))

(defmacro with-time-dependent-test-classes (&body forms)
  (with-unique-names (body)
    `(flet ((,body ()
              ,@forms))
       (bind ((*time-dependent-class-name* 'time-dependent-unbound-test))
         (,body))
       (bind ((*time-dependent-class-name* 'time-dependent-null-test))
         (,body)))))

(deftest test/tesites/time-dependent/table ()
  (with-time-dependent-test-classes
    (ensure-finalized (find-class *time-dependent-class-name*))
    (is (null (columns-of (find-slot *time-dependent-class-name* 'population))))))

(deftest test/tesites/time-dependent/initial-value/unbound/1 ()
  (with-transaction
    (signals unbound-slot-t (population-of (make-instance 'time-dependent-unbound-test)))))

(deftest test/tesites/time-dependent/initial-value/unbound/2 ()
  (with-transaction
    (bind ((instance (make-instance 'time-dependent-unbound-test)))
      (with-validity-range +beginning-of-time+ "2007-01-01"
        (setf (population-of instance) 1000))
      (with-validity-range "2008-01-01" +end-of-time+
        (setf (population-of instance) 2000))
      (with-validity-range "2007-01-01" "2008-01-01"
        (signals unbound-slot-t (population-of instance)))
      (with-validity-range "2006-01-01" "2008-01-01"
        (signals unbound-slot-t (population-of instance)))
      (with-validity-range "2007-01-01" "2009-01-01"
        (signals unbound-slot-t (population-of instance))))))

(deftest test/tesites/time-dependent/initial-value/null/1 ()
  (with-transaction
    (is (null (population-of (make-instance 'time-dependent-null-test))))))

(deftest test/tesites/time-dependent/initial-value/null/2 ()
  (with-transaction
    (bind ((instance (make-instance 'time-dependent-null-test)))
      (with-validity-range +beginning-of-time+ "2007-01-01"
        (setf (population-of instance) 1000))
      (with-validity-range "2008-01-01" +end-of-time+
        (setf (population-of instance) 2000))
      (with-validity-range "2007-01-01" "2008-01-01"
        (is (null (population-of instance))))
      (with-validity-range "2006-01-01" "2008-01-01"
        (is (values-having-validity=
             (make-values-having-validity (list 1000 nil)
                                          (list (parse-timestring "2006-01-01TZ") (parse-timestring "2007-01-01TZ"))
                                          (list (parse-timestring "2007-01-01TZ") (parse-timestring "2008-01-01TZ")))
             (population-of instance))))
      (with-validity-range "2007-01-01" "2009-01-01"
        (is (values-having-validity=
             (make-values-having-validity (list nil 2000)
                                          (list (parse-timestring "2007-01-01TZ") (parse-timestring "2008-01-01TZ"))
                                          (list (parse-timestring "2008-01-01TZ") (parse-timestring "2009-01-01TZ")))
             (population-of instance)))))))

(deftest test/tesites/time-dependent/initial-value/integer/1 ()
  (with-time-dependent-test-classes
    (with-validity "2007-01-01"
      (with-one-and-two-transactions
          (make-instance *time-dependent-class-name* :population 1000)
        (is (= 1000 (population-of -instance-)))))))

(deftest test/tesites/time-dependent/store-value/1 ()
  (with-time-dependent-test-classes
    (with-validity "2007-01-01"
      (with-one-and-two-transactions
          (bind ((instance (make-instance *time-dependent-class-name*)))
            (setf (population-of instance) 1000)
            instance)
        (is (= 1000 (population-of -instance-)))))))

(deftest test/tesites/time-dependent/store-value/2 ()
  (with-time-dependent-test-classes
    (with-validity "2007-01-01"
      (with-one-and-two-transactions
          (bind ((instance (make-instance *time-dependent-class-name*)))
            (setf (population-of instance) 1000)
            (is (= (update-counter-of (command-counter-of *transaction*)) 1))
            (is (= (insert-counter-of (command-counter-of *transaction*)) (+ 1 3)))
            instance)
        (let ((update-counter (update-counter-of (command-counter-of *transaction*)))
              (insert-counter (insert-counter-of (command-counter-of *transaction*))))
          (setf (population-of -instance-) 1000)
          (is (= (update-counter-of (command-counter-of *transaction*)) (1+ update-counter)))
          (is (= (insert-counter-of (command-counter-of *transaction*)) insert-counter)))))))

(deftest test/tesites/time-dependent/store-value/3 ()
  (with-time-dependent-test-classes
    (with-one-and-two-transactions
        (bind ((instance (make-instance *time-dependent-class-name*)))
          (with-validity "2007"
            (setf (population-of instance) 1000))
          instance)
      (with-validity "2007-07"
        (setf (population-of -instance-) 2000))
      (with-validity "2007-07"
        (is (= 2000 (population-of -instance-))))
      (with-validity "2007-07-01"
        (is (= 2000 (population-of -instance-))))
      (with-validity "2007-06"
        (is (= 1000 (population-of -instance-))))
      (with-validity "2007-08"
        (is (= 1000 (population-of -instance-)))))))

(deftest test/tesites/time-dependent/values-having-validity/1 ()
  (with-one-and-two-transactions
      (bind ((instance (make-instance 'time-dependent-unbound-test)))
        (with-validity "2006"
          (setf (population-of instance) 2006))
        instance)
    (with-validity "2007"
      (setf (population-of -instance-) 2007))
    (with-validity-range "2006" "2008"
      (iter (for index :from 0)
            (for (value validity-start validity-end) :in-values-having-validity (population-of -instance-))
            (ecase index
              (0 (is (= 2006 value))
                 (is (local-time= (parse-datestring "2006-01-01") validity-start))
                 (is (local-time= (parse-datestring "2007-01-01") validity-end)))
              (1 (is (= 2007 value))
                 (is (local-time= (parse-datestring "2007-01-01") validity-start))
                 (is (local-time= (parse-datestring "2008-01-01") validity-end))))))))

(defpclass* time-dependent-complex-test ()
  ((slot :type (or null integer-32))
   (slot-1 :type (or null integer-32) :time-dependent #t)
   (slot-2 :type (or null integer-32) :time-dependent #t)))

(deftest test/tesites/time-dependent/complex/same-validity ()
  (with-one-and-two-transactions
      (with-validity "2007"
        (bind ((instance
                (make-instance 'time-dependent-complex-test :slot 0 :slot-1 1000)))
          (setf (slot-2-of instance) 2000)
          instance))
    (with-validity "2007"
      (is (= 0 (slot-of -instance-)))
      (is (= 1000 (slot-1-of -instance-)))
      (is (= 2000 (slot-2-of -instance-)))
      (is (= 1 (length (h-objects-of -instance-)))))))

(deftest test/tesites/time-dependent/complex/different-validity ()
  (bind ((instance
          (with-transaction
            (with-validity "2007-01"
              (make-instance 'time-dependent-complex-test :slot 0 :slot-1 1000)))))
    (with-transaction
      (with-validity "2007-02"
        (with-revived-instance instance
          (is (= 0 (slot-of instance)))
          (is (null (slot-1-of instance)))
          (is (null (slot-2-of instance)))
          (setf (slot-2-of instance) 2000)
          (is (= 2 (length (h-objects-of instance)))))))
    (with-transaction
      (with-validity "2007-01"
        (with-revived-instance instance
          (is (= 0 (slot-of instance)))
          (is (= 1000 (slot-1-of instance)))
          (is (is (null (slot-2-of instance)))))))))
