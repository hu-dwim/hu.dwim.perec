(in-package :cl-perec-test)

;;;;;
;;; T

(defsuite* (test/tesites/t :in test/tesites))

(defpclass* t-test ()
  ((population :type integer-32 :time-dependent #t :temporal #t))
  (:metaclass persistent-class-t))

(defpclass* t-test-h ()
  ((t-value :type timestamp)
   (validity-start :type timestamp)
   (validity-end :type timestamp)
   (population :type (or unbound integer-32))))

(defassociation*
  ((:class t-test-h :slot t-test :type t-test)
   (:class t-test :slot t-hs :type (set t-test-h))))

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
