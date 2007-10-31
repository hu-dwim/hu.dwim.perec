(in-package :cl-perec-test)

(defsuite* (test/query/subselect :in test/query))


(defpclass* subselect-parent-test ()
  ())

(defpclass* subselect-child-test ()
  ((int-attr :type (or null integer-32))))

(defassociation*
  ((:class subselect-parent-test :slot children :type (set subselect-child-test))
   (:class subselect-child-test :slot parent :type subselect-parent-test)))

(defixture subselect-data
  (with-transaction
    (purge-instances 'subselect-parent-test)
    (purge-instances 'subselect-child-test)
    (bind ((p1 (make-instance 'subselect-parent-test))
           (p2 (make-instance 'subselect-parent-test))
           (p3 (make-instance 'subselect-parent-test)))
      (make-instance 'subselect-child-test :parent p1 :int-attr 1)
      (make-instance 'subselect-child-test :parent p1 :int-attr 2)
      (make-instance 'subselect-child-test :parent p2 :int-attr 3)
      (make-instance 'subselect-child-test :parent p2 :int-attr 4)
      (make-instance 'subselect-child-test :parent p3 :int-attr 5)
      (make-instance 'subselect-child-test :parent p3 :int-attr 6))))

(defmacro def-subselect-test (name (&rest args) &body body)
  `(deftest ,name ,args
    (with-setup subselect-data
      (with-transaction
        ,@body))))

#+nil
(def-subselect-test test/query/subselect/int ()
  (select (parent child)
    (from (parent subselect-parent-test) (child subselect-child-test))
    (where (and (eq (parent-of child) parent)
                (= (int-attr-of child) (select (max (int-attr-of c))
                                         (from (c subselect-child-test))
                                         (where (eq (parent-of c) parent))))))))



