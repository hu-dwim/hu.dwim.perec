(in-package :cl-perec-test)

(defsuite* (test/persistence/event :in test/persistence))

(defpclass* event-test ()
  ((code :type (or null integer-16))))

(deftest test/persistence/event/created/1 ()
  (with-transaction
    (let ((instance (make-instance 'event-test)))
      (is (created-p instance))
      (is (not (modified-p instance)))
      (is (not (deleted-p instance))))))

(deftest test/persistence/event/created/2 ()
  (with-transaction
    (let ((instance (make-instance 'event-test)))
      (purge-instance instance)
      (is (not (created-p instance)))
      (is (not (modified-p instance)))
      (is (not (deleted-p instance))))))

(deftest test/persistence/event/created/3 ()
  (with-transaction
    (let ((instance (make-instance 'event-test)))
      (make-transient instance)
      (is (not (created-p instance)))
      (is (not (modified-p instance)))
      (is (not (deleted-p instance))))))

(deftest test/persistence/event/not-modified/1 ()
  (let ((instance
         (with-transaction
           (make-instance 'event-test))))
    (with-transaction
      (revive-instance instance)
      (is (not (created-p instance)))
      (is (not (modified-p instance)))
      (is (not (deleted-p instance))))))

(deftest test/persistence/event/modified/1 ()
  (let ((instance
         (with-transaction
           (make-instance 'event-test))))
    (with-transaction
      (revive-instance instance)
      (setf (code-of instance) 1)
      (is (not (created-p instance)))
      (is (modified-p instance))
      (is (not (deleted-p instance))))))

(deftest test/persistence/event/modified/2 ()
  (let ((instance
         (with-transaction
           (make-instance 'event-test))))
    (with-transaction
      (revive-instance instance)
      (setf (code-of instance) 1)
      (purge-instance instance)
      (is (not (created-p instance)))
      (is (not (modified-p instance)))
      (is (deleted-p instance)))))

(deftest test/persistence/event/deleted/1 ()
  (let ((instance
         (with-transaction
           (make-instance 'event-test))))
    (with-transaction
      (revive-instance instance)
      (purge-instance instance)
      (is (not (created-p instance)))
      (is (not (modified-p instance)))
      (is (deleted-p instance)))))
