(in-package :cl-perec-test)

(defsuite* test/transaction :in test/persistence)

(defpclass* transaction-test ()
  ((name nil :type t)))

(deftest test/transaction/return-value/1 ()
  (is (eq 2 (with-transaction 1 2))))
    
(deftest test/transaction/in-transaction/1 ()
  (with-transaction
    (is (prc::object-in-transaction-p (make-instance 'transaction-test)))))

(deftest test/transaction/in-transaction/2 ()
  (is (not (prc::object-in-transaction-p
            (with-transaction
              (make-instance 'transaction-test))))))

(deftest test/transaction/revive-object/1 ()
  (let ((object
         (with-transaction
           (make-instance 'transaction-test))))
    (finishes
      (with-transaction
        (revive-object object)
        (name-of object)))))

(deftest test/transaction/revive-object/2 ()
  (let ((object
         (with-transaction
           (make-instance 'transaction-test))))
    (signals error
      (with-transaction
        (name-of object)))))
