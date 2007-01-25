(in-package :cl-perec-test)

(defsuite* test/persistency :in test)

(defpclass* persistency-test ()
  ((name :type (string 20))))

(deftest test/persistency/make-instance/1 ()
  (with-transaction
    (is (persistent-p (make-instance 'persistency-test :name "the one")))))

(deftest test/persistency/make-instance/2 ()
  (let ((object
         (with-transaction
           (make-instance 'persistency-test :name "the one"))))
    (with-transaction
      (revive-object object)
      (is (persistent-p object)))))

(deftest test/persistency/make-transient/1 ()
  (let ((object (make-instance 'persistency-test :name "the one" :persistent #f)))
    (is (not (persistent-p object)))))

(deftest test/persistency/make-transient/2 ()
  (with-transaction
    (let ((object (make-instance 'persistency-test :name "the one")))
      (make-transient object)
      (is (not (persistent-p object))))))

(deftest test/persistency/make-transient/3 ()
  (with-transaction
    (let ((object (make-instance 'persistency-test :name "the one")))
      (execute "delete from _persistency_test")
      (slot-makunbound object 'prc::persistent)
      (is (not (persistent-p object))))))
  
(deftest test/persistency/make-persistent/1 ()
  (with-transaction
    (let ((object (make-instance 'persistency-test :name "the one")))
      (make-transient object)
      (make-persistent object)
      (is (persistent-p object))
      (is (equal (name-of object) "the one")))))

(deftest test/persistency/make-persistent/2 ()
  (with-transaction
    (let ((object (make-instance 'persistency-test :name "the one")))
      (slot-makunbound object 'prc::persistent)
      (is (persistent-p object)))))
