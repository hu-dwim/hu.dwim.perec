(in-package :cl-perec-test)

(defsuite* (test/persistence/inheritance :in test/persistence))

(defpclass* inheritance-t1-test ()
  ((slot :type (string 20) :cached #f)))

(defpclass* inheritance-t2-test (inheritance-t1-test)
  ((slot :cached #t)))

(deftest test/persistence/inheritance/store-value/1 ()
  (let ((value "hello"))
    (with-transaction
      (is (equal value (slot-of (make-instance 'inheritance-t1-test :slot value))))
      (is (equal value (slot-of (make-instance 'inheritance-t2-test :slot value)))))))

(deftest test/persistence/inheritance/override/1 ()
  (is (not (prc::cached-p (prc::find-slot (find-class 'inheritance-t1-test) 'slot))))
  (is (prc::cached-p (prc::find-slot (find-class 'inheritance-t2-test) 'slot))))
