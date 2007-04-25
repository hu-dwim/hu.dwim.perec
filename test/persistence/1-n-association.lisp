(in-package :cl-perec-test)

(defvar *association-1-n-parent-class-name* 'parent-test)

(defvar *association-1-n-child-class-name* 'child-test)

(defsuite* (test/persistence/association/1-n :in test/persistence/association) ()
  (flet ((body ()
           (with-and-without-caching-slot-values
             (run-child-tests))))
    (body)
    (bind ((*association-1-n-parent-class-name* '1-n-self-association-test)
           (*association-1-n-child-class-name* '1-n-self-association-test))
      (body))))

(defpclass* parent-test ()
  ())
   
(defpclass* child-test ()
  ())

(defpclass* 1-n-self-association-test ()
  ())

(defassociation*
  ((:class 1-n-self-association-test :slot children :type (set 1-n-self-association-test))
   (:class 1-n-self-association-test :slot parent :type (or null 1-n-self-association-test))))

(defassociation*
  ((:class child-test :slot parent :type (or null parent-test))
   (:class parent-test :slot children :type (set child-test))))

(defmacro with-parent-and-child-transaction (&body body)
  `(with-transaction
    (bind ((parent (make-instance *association-1-n-parent-class-name*))
           (child (make-instance *association-1-n-child-class-name*)))
      ,@body)))

(deftest test/persistence/association/1-n/class ()
  (prc::ensure-exported (find-class *association-1-n-child-class-name*))
  (prc::ensure-exported (find-class *association-1-n-parent-class-name*))
  (let ((parent-slot (prc::find-slot *association-1-n-child-class-name* 'parent))
        (children-slot (prc::find-slot *association-1-n-parent-class-name* 'children)))
    (is (prc::primary-table-slot-p parent-slot))
    (is (prc::data-table-slot-p parent-slot))
    (is (not (prc::primary-table-slot-p children-slot)))
    (is (not (prc::data-table-slot-p children-slot)))
    (is (cache-p parent-slot))
    (is (not (cache-p children-slot)))))

(deftest test/persistence/association/1-n/initial-value/1 ()
  (with-parent-and-child-transaction
    (is (eq nil (parent-of child)))
    (is (= 0 (size (children-of parent))))))
   
(deftest test/persistence/association/1-n/initial-value/2 ()
  (with-transaction
    (bind ((parent (make-instance *association-1-n-parent-class-name*))
           (child (make-instance *association-1-n-child-class-name* :parent parent)))
      (is (eq (parent-of child) parent)))))

(deftest test/persistence/association/1-n/initial-value/3 ()
  (with-transaction
    (bind ((child (make-instance *association-1-n-child-class-name*))
           (parent (make-instance *association-1-n-parent-class-name* :children (list child))))
      (is (equal (children-of parent) (list child))))))

(deftest test/persistence/association/1-n/store-value/1 ()
  (with-parent-and-child-transaction
    (setf (parent-of child) parent)
    (is (eq parent (parent-of child)))))

(deftest test/persistence/association/1-n/store-value/2 ()
  (with-parent-and-child-transaction
    (setf (children-of parent) (list child))
    (is (equal (list child) (children-of parent)))))

(deftest test/persistence/association/1-n/referential-integrity/1 ()
  (with-parent-and-child-transaction
    (setf (parent-of child) parent)
    (bind ((children (children-of parent)))
      (is (= 1 (size children)))
      (is (equal (list child) children)))))

(deftest test/persistence/association/1-n/referential-integrity/2 ()
  (with-parent-and-child-transaction
    (setf (children-of parent) (list child))
    (is (eq parent (parent-of child)))))

(deftest test/persistence/association/1-n/referential-integrity/3 ()
  (with-parent-and-child-transaction
    (setf (children-of parent) (list child))
    (setf (parent-of child) (make-instance *association-1-n-parent-class-name*))
    (is (= 0 (size (children-of parent))))))

(deftest test/persistence/association/1-n/referential-integrity/4 ()
  (with-parent-and-child-transaction
    (setf (parent-of child) parent)
    (setf (children-of parent) (list (make-instance *association-1-n-child-class-name*)))
    (is (eq nil (parent-of child)))))

(deftest test/persistence/association/1-n/collection/1 ()
  (with-parent-and-child-transaction
    (bind ((children (children-of* parent)))
      (insert-item children child)
      (is (= 1 (size children)))
      (is (equal (list child) (children-of parent)))
      (delete-item children child)
      (is (= 0 (size children)))
      (is (null (children-of parent))))))

(deftest test/persistence/association/1-n/collection/2 ()
  (with-parent-and-child-transaction
    (bind ((children (children-of* parent))
           (other-child (make-instance *association-1-n-child-class-name*)))
      (insert-item children child)
      (insert-item children other-child)
      (delete-item children child)
      (is (= 1 (size children)))
      (is (equal (list other-child) (list-of children))))))
