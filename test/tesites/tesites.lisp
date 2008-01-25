(in-package :cl-perec-test)

;;;;;
;;; T

(defsuite* (test/tesites/t :in test/tesites))

(defpclass* t-test (t-object)
  ((population :type integer-32 :time-dependent #t :temporal #t))
  (:metaclass persistent-class-t))

(defpclass* t-test-h (time-dependent-object temporal-object)
  ((population :type (or unbound integer-32))))

(defassociation*
  ((:class t-test-h :slot t-object :type t-test)
   (:class t-test :slot h-objects :type (set t-test-h))))

(defsuite* (test/tesites/temporal-time-dependent :in test/tesites))

(defpclass* t-a-test (t-object)
  ((a-value :type integer-32 :time-dependent #t))
  (:metaclass persistent-class-t))

(defpclass* t-a-test-h (time-dependent-object)
  ((a-value :type (or unbound integer-32))))

(defassociation*
  ((:class t-a-test-h :slot t-object :type t-a-test)
   (:class t-a-test :slot h-objects :type (set t-a-test-h))))

(defpclass* t-b-test (t-a-test)
  ((b-value :type integer-32 :temporal #t :time-dependent #t))
  (:metaclass persistent-class-t))

(defpclass* t-b-test-h (t-a-test-h temporal-object)
  ((b-value :type (or unbound integer-32))))

(defassociation*
  ((:class t-b-test-h :slot t-object :type t-b-test)
   (:class t-b-test :slot h-objects :type (set t-b-test-h))))

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


