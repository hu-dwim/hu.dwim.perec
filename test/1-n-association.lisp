(in-package :cl-perec-test)

(in-suite test/association)

(defsuite test/association/1-n)

(in-suite test/association/1-n)

(defmacro with-transaction-for-parent-and-child (&body body)
  `(with-transaction
    (bind ((parent (make-instance 'parent))
           (child (make-instance 'child)))
      ,@body)))

(defpclass* parent ()
  ())
   
(defpclass* child ()
  ())
   
(defassociation*
  ((parent :0..1)
   (child :0..N)))

(deftest test/association/1-n/initial-value/1 ()
  (with-transaction-for-parent-and-child
      (is (eq nil (parent-of child)))
    (is (= 0 (size (children-of parent))))))
   
(deftest test/association/1-n/initial-value/2 ()
  (with-transaction
    (bind ((parent (make-instance 'parent))
           (child (make-instance 'child :parent parent)))
      (is (eq (parent-of child) parent)))))

(deftest test/association/1-n/initial-value/3 ()
  (with-transaction
    (bind ((child (make-instance 'child))
           (parent (make-instance 'parent :children (list child))))
      (is (equal (children-of parent) (list child))))))

(deftest test/association/1-n/store-value/1 ()
  (with-transaction-for-parent-and-child
      (setf! (parent-of child) parent)
    (is (eq parent (parent-of child)))))

(deftest test/association/1-n/store-value/2 ()
  (with-transaction-for-parent-and-child
      (setf! (children-of parent) (list child))
    (is (equal (list child) (children-of parent)))))

(deftest test/association/1-n/referential-integrity/1 ()
  (with-transaction-for-parent-and-child
      (setf! (parent-of child) parent)
    (bind ((children (children-of parent)))
      (is (= 1 (size children)))
      (is (equal (list child) children)))))

(deftest test/association/1-n/referential-integrity/2 ()
  (with-transaction-for-parent-and-child
      (setf! (children-of parent) (list child))
    (is (eq parent (parent-of child)))))

(deftest test/association/1-n/referential-integrity/3 ()
  (with-transaction-for-parent-and-child
      (setf! (children-of parent) (list child))
    (setf! (parent-of child) (make-instance 'parent))
    (is (= 0 (size (children-of parent))))))

(deftest test/association/1-n/referential-integrity/4 ()
  (with-transaction-for-parent-and-child
      (setf! (parent-of child) parent)
    (setf! (children-of parent) (list (make-instance 'child)))
    (is (eq nil (parent-of child)))))

(deftest test/association/1-n/collection/1 ()
  (with-transaction-for-parent-and-child
      (bind ((children (children-of* parent)))
        (insert-item children child)
        (is (= 1 (size children)))
        (is (equal (list child) (children-of parent)))
        (delete-item children child)
        (is (= 0 (size children)))
        (is (null (children-of parent))))))

(deftest test/association/1-n/collection/2 ()
  (with-transaction-for-parent-and-child
      (bind ((children (children-of* parent))
             (other-child (make-instance 'child)))
        (insert-item children child)
        (insert-item children other-child)
        (delete-item children child)
        (is (= 1 (size children)))
        (is (equal (list other-child) (list-of children))))))
