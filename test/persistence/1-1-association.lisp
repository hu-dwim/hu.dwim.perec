(in-package :cl-perec-test)

(defsuite* (test/persistence/association :in test/persistence))

(defvar *association-1-1-brother-class-name* 'brother-test)

(defvar *association-1-1-sister-class-name* 'sister-test)

(defsuite* test/persistence/association/1-1 ()
  (flet ((body ()
           (with-and-without-caching-slot-values
             (run-child-tests))))
    (body)
    (bind ((*association-1-1-brother-class-name* 'strict-brother-test)
           (*association-1-1-sister-class-name* 'strict-sister-test))
      (test/persistence/association/1-1/initial-value/2)
      (test/persistence/association/1-1/initial-value/3)
      (test/persistence/association/1-1/store-value/1)
      (test/persistence/association/1-1/store-value/2)
      (test/persistence/association/1-1/referential-integrity/1)
      (test/persistence/association/1-1/referential-integrity/2))
    (bind ((*association-1-1-brother-class-name* '1-1-self-association-test)
           (*association-1-1-sister-class-name* '1-1-self-association-test))
      (body))
    (bind ((*association-1-1-brother-class-name* 'concrete-brother-test)
           (*association-1-1-sister-class-name* 'concrete-sister-test))
      (body))))

(defpclass* brother-test ()
  ())
   
(defpclass* sister-test ()
  ())

(defassociation*
  ((:class brother-test :slot sister :type (or null sister-test))
   (:class sister-test :slot brother :type (or null brother-test))))

(defpclass* strict-brother-test ()
  ())
   
(defpclass* strict-sister-test ()
  ())

(defassociation*
  ((:class strict-brother-test :slot sister :type strict-sister-test)
   (:class strict-sister-test :slot brother :type strict-brother-test)))

(defpclass* unbound-brother-test ()
  ())
   
(defpclass* unbound-sister-test ()
  ())

(defassociation*
  ((:class unbound-brother-test :slot sister :type (or unbound unbound-sister-test))
   (:class unbound-sister-test :slot brother :type (or unbound unbound-brother-test))))

(defpclass* 1-1-self-association-test ()
  ())

(defassociation*
  ((:class 1-1-self-association-test :slot sister :type (or null 1-1-self-association-test))
   (:class 1-1-self-association-test :slot brother :type (or null 1-1-self-association-test))))

(defpclass* abstract-brother-test ()
  ()
  (:abstract #t)
  (:direct-store :push-down))

(defpclass* concrete-brother-test (abstract-brother-test)
  ())
   
(defpclass* abstract-sister-test ()
  ()
  (:abstract #t)
  (:direct-store :push-down))

(defpclass* concrete-sister-test (abstract-sister-test)
  ())

(defassociation*
  ((:class abstract-brother-test :slot sister :type (or null abstract-sister-test))
   (:class abstract-sister-test :slot brother :type (or null abstract-brother-test))))

(defmacro with-sister-and-brother-transaction (&body body)
  `(with-transaction
    (bind ((sister (make-instance *association-1-1-sister-class-name*))
           (brother (make-instance *association-1-1-brother-class-name*)))
      ,@body)))

(deftest test/persistence/association/1-1/class ()
  (ensure-exported (find-class *association-1-1-brother-class-name*))
  (ensure-exported (find-class *association-1-1-sister-class-name*))
  (let ((sister-slot (find-slot *association-1-1-brother-class-name* 'sister))
        (brother-slot (find-slot *association-1-1-sister-class-name* 'brother)))
    (is (primary-table-slot-p sister-slot))
    (is (data-table-slot-p sister-slot))
    (is (not (primary-table-slot-p brother-slot)))
    (is (not (data-table-slot-p brother-slot)))
    (is (cache-p sister-slot))
    (is (cache-p brother-slot))))

(deftest test/persistence/association/1-1/initial-value/1 ()
  (with-sister-and-brother-transaction
    (is (eq nil (brother-of sister)))
    (is (eq nil (sister-of brother)))))

(deftest test/persistence/association/1-1/initial-value/2 ()
  (with-transaction
    (bind ((sister (make-instance *association-1-1-sister-class-name*))
           (brother (make-instance *association-1-1-brother-class-name* :sister sister)))
      (is (eq (sister-of brother) sister))
      (is (eq (brother-of sister) brother)))))

(deftest test/persistence/association/1-1/initial-value/3 ()
  (with-transaction
    (bind ((brother (make-instance *association-1-1-brother-class-name*))
           (sister (make-instance *association-1-1-sister-class-name* :brother brother)))
      (is (eq (brother-of sister) brother))
      (is (eq (sister-of brother) sister)))))

(deftest test/persistence/association/1-1/store-value/1 ()
  (with-sister-and-brother-transaction
    (setf (brother-of sister) brother)
    (is (eq brother (brother-of sister)))))

(deftest test/persistence/association/1-1/store-value/2 ()
  (with-sister-and-brother-transaction
    (setf (sister-of brother) sister)
    (is (eq sister (sister-of brother)))))

(deftest test/persistence/association/1-1/referential-integrity/1 ()
  (with-sister-and-brother-transaction
    (setf (brother-of sister) brother)
    (is (eq sister (sister-of brother)))))

(deftest test/persistence/association/1-1/referential-integrity/2 ()
  (with-sister-and-brother-transaction
    (setf (sister-of brother) sister)
    (is (eq brother (brother-of sister)))))

(deftest test/persistence/association/1-1/referential-integrity/3 ()
  (with-sister-and-brother-transaction
    (setf (sister-of brother) sister)
    (setf (sister-of brother) nil)
    (is (eq nil (sister-of brother)))
    (is (eq nil (brother-of sister)))))

(deftest test/persistence/association/1-1/referential-integrity/4 ()
  (with-sister-and-brother-transaction
    (setf (brother-of sister) brother)
    (setf (brother-of sister) nil)
    (is (eq nil (sister-of brother)))
    (is (eq nil (brother-of sister)))))
   
(deftest test/persistence/association/1-1/referential-integrity/5 ()
  (with-sister-and-brother-transaction
    (setf (sister-of brother) sister)
    (setf (sister-of brother) (make-instance *association-1-1-sister-class-name*))
    (is (eq nil (brother-of sister)))
    (setf (brother-of sister) brother)
    (setf (brother-of sister) (make-instance *association-1-1-brother-class-name*))
    (is (eq nil (sister-of brother)))))

(deftest test/persistence/association/1-1/referential-integrity/6 ()
  (with-transaction
    (bind ((sister-1 (make-instance *association-1-1-sister-class-name*))
           (sister-2 (make-instance *association-1-1-sister-class-name*))
           (brother-1 (make-instance *association-1-1-brother-class-name*))
           (brother-2 (make-instance *association-1-1-brother-class-name*)))
      (setf (brother-of sister-1) brother-1)
      (setf (brother-of sister-2) brother-1)
      (setf (brother-of sister-2) brother-2)
      (is (eq nil (brother-of sister-1)))
      (is (eq brother-2 (brother-of sister-2)))
      (is (eq nil (sister-of brother-1)))
      (is (eq sister-2 (sister-of brother-2))))))

(deftest test/persistence/association/1-1/referential-integrity/7 ()
  (with-transaction
    (bind ((sister-1 (make-instance *association-1-1-sister-class-name*))
           (sister-2 (make-instance *association-1-1-sister-class-name*))
           (brother-1 (make-instance *association-1-1-brother-class-name*))
           (brother-2 (make-instance *association-1-1-brother-class-name*)))
      (setf (sister-of brother-1) sister-1)
      (setf (sister-of brother-2) sister-1)
      (setf (sister-of brother-2) sister-2)
      (is (eq nil (sister-of brother-1)))
      (is (eq sister-2 (sister-of brother-2)))
      (is (eq nil (brother-of sister-1)))
      (is (eq brother-2 (brother-of sister-2))))))
