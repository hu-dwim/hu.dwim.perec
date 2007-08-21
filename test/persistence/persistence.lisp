(in-package :cl-perec-test)

(in-suite test/persistence)

(defpclass* persistence-test ()
  ((name :type (text 20))))

(deftest test/persistence/make-instance/1 ()
  (with-transaction
    (is (persistent-p (make-instance 'persistence-test :name "the one")))))

(deftest test/persistence/make-instance/2 ()
  (let ((instance
         (with-transaction
           (make-instance 'persistence-test :name "the one"))))
    (with-transaction
      (revive-instance instance)
      (is (persistent-p instance)))))

(deftest test/persistence/make-transient/1 ()
  (let ((instance (make-instance 'persistence-test :name "the one" :persistent #f)))
    (is (not (persistent-p instance)))))

(deftest test/persistence/make-transient/2 ()
  (with-transaction
    (let ((instance (make-instance 'persistence-test :name "the one")))
      (make-transient instance)
      (is (not (persistent-p instance))))))

(deftest test/persistence/make-transient/3 ()
  (with-transaction
    (let ((instance (make-instance 'persistence-test :name "the one")))
      (execute (sql `(delete ,(rdbms-name-for 'persistence-test))))
      (slot-makunbound instance 'persistent)
      (is (not (persistent-p instance))))))
  
(deftest test/persistence/make-persistent/1 ()
  (with-transaction
    (let ((instance (make-instance 'persistence-test :name "the one")))
      (make-transient instance)
      (make-persistent instance)
      (is (persistent-p instance))
      (is (equal (name-of instance) "the one")))))

(deftest test/persistence/make-persistent/2 ()
  (with-transaction
    (let ((instance (make-instance 'persistence-test :name "the one")))
      (slot-makunbound instance 'persistent)
      (is (persistent-p instance)))))

(deftest test/persistence/lock-instance/1 ()
  (with-one-and-two-transactions
      (make-instance 'persistence-test :name "the one")
    (is (lock-instance -instance- :wait #t)))
  (with-one-and-two-transactions
      (make-instance 'persistence-test :name "the one")
    (is (lock-instance -instance- :wait #f))))

(deftest test/persistence/lock-instance/2 ()
  (let ((instance
         (with-transaction
           (make-instance 'persistence-test :name "the one"))))
    (with-transaction
      (with-reloaded-instance instance
        (lock-instance instance :wait #t))
      (is (not
           (with-transaction
             (with-reloaded-instance instance
               (lock-instance instance :wait #f))))))))

(defpclass* initform-1-test ()
  ((name "Hello" :type (text 20))))

(defpclass* initform-2-test ()
  ((name (error "Hello") :type (text 20))))

(deftest test/persistence/initform/1 ()
  (with-transaction
    (is (equal "Hello" (name-of (make-instance 'initform-1-test))))))

(deftest test/persistence/initform/2 ()
  (let ((instance
         (with-transaction
           (stefil:signals error (make-instance 'initform-2-test)))))
    (with-transaction
      (finishes (revive-instance instance)))))
