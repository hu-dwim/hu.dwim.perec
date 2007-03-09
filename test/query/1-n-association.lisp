(in-package :cl-perec-test)

(defsuite* (test/query/select/association/1-n :in test/query/select/association))

(defmacro with-parent-and-child-in-transaction (&body body)
  `(with-transaction
    (purge-instances 'child-test)
    (purge-instances 'parent-test)
    (bind ((parent (make-instance 'parent-test))
           (child (make-instance 'child-test)))
      ,@body)))

(deftest test/query/select/association/1-n/1 ()
  (with-parent-and-child-in-transaction
    (is (null (select ((p parent-test))
                (assert (eq p (parent-of child)))
                (collect p))))
    (is (null (select ((c child-test))
                     (assert (member c (children-of parent)))
                     (collect c))))))

(deftest test/query/select/association/1-n/2 ()
  (with-parent-and-child-in-transaction
    (is (equal (select ((p parent-test))
                 (assert (null (children-of p)))
                 (collect p))
               (list parent)))
    (is (equal (select ((c child-test))
                 (assert (null (parent-of c)))
                 (collect c))
               (list child)))))

(deftest test/query/select/association/1-n/3 ()
  (with-parent-and-child-in-transaction
    (setf (parent-of child) parent)
    (is (equal (select ((p parent-test))
                 (assert (eq p (parent-of child)))
                 (collect p))
               (list parent)))
    (is (equal (select ((c child-test))
                 (assert (member c (children-of parent)))
                 (collect c))
               (list child)))))

(deftest test/query/select/association/1-n/4 ()
  (with-parent-and-child-in-transaction
    (setf (parent-of child) parent)
    (is (null (select ((p parent-test))
                (assert (null (children-of p)))
                (collect p))))
    (is (null (select ((c child-test))
                (assert (null (parent-of c)))
                (collect c))))))