(in-package :cl-perec-test)

(defsuite* test/set :in test)

(defpclass* referred-set-test ()
  ())

(defpclass* reference-set-test ()
  ((referred-set :type (set referred-set-test))))

(defmacro with-transaction-for-reference-set (&body body)
  `(with-transaction
    (let ((referred (make-instance 'referred-set-test))
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
