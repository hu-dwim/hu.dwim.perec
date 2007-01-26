(in-package :cl-perec-test)

(defsuite* test/set :in test/persistence)

(defpclass* referred-set-test ()
  ())

;; TODO: remove this nil
(defpclass* reference-set-test ()
  ((referred-set nil :type (set referred-set-test))))

(defmacro with-reference-set-transaction (&body body)
  `(with-transaction
    (let ((referred (make-instance 'referred-set-test))
          (reference-set (make-instance 'reference-set-test)))
      (declare (ignorable referred reference-set))
      ,@body)))

(deftest test/set/initial-value/1 ()
  (with-reference-set-transaction
    (is (eq nil (referred-set-of reference-set)))))

(deftest test/set/initial-value/2 ()
  (with-transaction
    (bind ((referred (make-instance 'referred-set-test))
           (reference-set (make-instance 'reference-set-test :referred-set (list referred))))
      (is (equal (referred-set-of reference-set) (list referred))))))

(deftest test/set/store-value/1 ()
  (with-reference-set-transaction
    (setf (referred-set-of reference-set) (list referred))
    (is (equal (list referred) (referred-set-of reference-set)))))

(deftest test/set/collection/1 ()
  (with-reference-set-transaction
    (bind ((referred-set (referred-set-of* reference-set)))
      (insert-item referred-set referred)
      (is (= 1 (size referred-set)))
      (is (equal (list referred) (referred-set-of reference-set)))
      (delete-item referred-set referred)
      (is (= 0 (size referred-set)))
      (is (null (referred-set-of reference-set))))))

(deftest test/set/collection/2 ()
  (with-reference-set-transaction
    (bind ((referred-set (referred-set-of* reference-set))
           (other-referred (make-instance 'referred-set-test)))
      (insert-item referred-set referred)
      (insert-item referred-set other-referred)
      (delete-item referred-set referred)
      (is (= 1 (size referred-set)))
      (is (equal (list other-referred) (list-of referred-set))))))
