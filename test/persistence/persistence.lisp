(in-package :cl-perec-test)

(in-suite test/persistence)

(defpclass* persistence-test ()
  ((name :type (string 20))))

(deftest test/persistence/make-instance/1 ()
  (with-transaction
    (is (persistent-p (make-instance 'persistence-test :name "the one")))))

(deftest test/persistence/make-instance/2 ()
  (let ((object
         (with-transaction
           (make-instance 'persistence-test :name "the one"))))
    (with-transaction
      (revive-object object)
      (is (persistent-p object)))))

(deftest test/persistence/make-transient/1 ()
  (let ((object (make-instance 'persistence-test :name "the one" :persistent #f)))
    (is (not (persistent-p object)))))

(deftest test/persistence/make-transient/2 ()
  (with-transaction
    (let ((object (make-instance 'persistence-test :name "the one")))
      (make-transient object)
      (is (not (persistent-p object))))))

(deftest test/persistence/make-transient/3 ()
  (with-transaction
    (let ((object (make-instance 'persistence-test :name "the one")))
      (execute "delete from _persistence_test")
      (slot-makunbound object 'prc::persistent)
      (is (not (persistent-p object))))))
  
(deftest test/persistence/make-persistent/1 ()
  (with-transaction
    (let ((object (make-instance 'persistence-test :name "the one")))
      (make-transient object)
      (make-persistent object)
      (is (persistent-p object))
      (is (equal (name-of object) "the one")))))

(deftest test/persistence/make-persistent/2 ()
  (with-transaction
    (let ((object (make-instance 'persistence-test :name "the one")))
      (slot-makunbound object 'prc::persistent)
      (is (persistent-p object)))))
