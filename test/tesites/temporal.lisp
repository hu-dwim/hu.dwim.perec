(in-package :cl-perec-test)

;;;;;;;;;;;;
;;; Temporal

(defsuite* (test/tesites/temporal :in test/tesites))

(defpclass* temporal-test (t-object)
  ((population :type integer-32 :temporal #t))
  (:metaclass persistent-class-t))

(defpclass* temporal-test-h (temporal-object)
  ((population :type (or unbound integer-32))))

(defassociation*
  ((:class temporal-test-h :slot t-object :type temporal-test)
   (:class temporal-test :slot h-objects :type (set temporal-test-h))))

(deftest test/tesites/temporal/t-not-specified ()
  (with-transaction
    (signals unbound-variable (population-of (make-instance 'temporal-test)))))

(deftest test/tesites/temporal/unbound/1 ()
  (with-transaction
    (with-default-t
      (signals unbound-slot-t (population-of (make-instance 'temporal-test))))))

(deftest test/tesites/temporal/store-value/1 ()
  (with-one-and-two-transactions
      (with-default-t
        (let ((instance (make-instance 'temporal-test)))
          (setf (population-of instance) 1000)
          instance))
    (with-default-t
      (is (= 1000 (population-of -instance-))))))

(deftest test/tesites/temporal/override-value/1 ()
  (multiple-value-bind (instance t-value)
      (with-transaction
        (with-default-t
          (let ((instance (make-instance 'temporal-test)))
            (setf (population-of instance) 1000)
            (values instance *t*))))
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
          (is (= 1000 (population-of instance))))))))

