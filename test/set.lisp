(in-package :cl-perec-test)

(in-suite test)

(defsuite test/set)

(in-suite test/set)

(defpclass* referred ()
  ())

(defpclass* reference-set-test ()
  ((referred-set :type (set referred))))

(defmacro with-transaction-for-reference-set (&body body)
  `(with-transaction
    (let ((referred (make-instance 'referred))
          ;; TODO: remove this nil
          (reference-set (make-instance 'reference-set-test :referred-set nil)))
      (declare (ignorable referred reference-set))
      ,@body)))

(deftest test/set/initial-value ()
  (with-transaction-for-reference-set
    (is (eq nil (referred-set-of reference-set)))))

(deftest test/set/store-value/1 ()
  (with-transaction-for-reference-set
    (setf (referred-set-of reference-set) (list referred))
    (is (equal (list referred) (referred-set-of reference-set)))))
