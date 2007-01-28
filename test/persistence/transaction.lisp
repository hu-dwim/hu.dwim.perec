(in-package :cl-perec-test)

(defsuite* test/persistence/transaction :in test/persistence)

(defpclass* transaction-test ()
  ((name nil :type t)))

(deftest test/persistence/transaction/return-value/1 ()
  (is (eq 2 (with-transaction 1 2))))
    
(deftest test/persistence/transaction/in-transaction/1 ()
  (with-transaction
    (is (object-in-transaction-p (make-instance 'transaction-test)))))

(deftest test/persistence/transaction/in-transaction/2 ()
  (is (not (object-in-transaction-p
            (with-transaction
              (make-instance 'transaction-test))))))

(deftest test/persistence/transaction/revive-object/1 ()
  (let ((object
         (with-transaction
           (make-instance 'transaction-test))))
    (finishes
      (with-transaction
        (revive-object object)
        (name-of object)))))

(deftest test/persistence/transaction/revive-object/2 ()
  (let ((object
         (with-transaction
           (make-instance 'transaction-test))))
    (signals error
      (with-transaction
        (name-of object)))))
