(in-package :cl-perec-test)

(defsuite* test/association :in test)

(defsuite* test/association/1-1 :in test/association)

(defpclass* brother ()
  ())
   
(defpclass* sister ()
  ())

(defassociation*
  ((brother :0..1)
   (sister :0..1)))

(defmacro with-transaction-for-sister-and-brother (&body body)
  `(with-transaction
    (bind ((sister (make-instance 'sister-test))
           (brother (make-instance 'brother-test)))
      ,@body)))

(deftest test/association/1-1/initial-value/1 ()
  (with-transaction-for-sister-and-brother
    (is (eq nil (brother-of sister)))
    (is (eq nil (sister-of brother)))))

(deftest test/association/1-1/initial-value/2 ()
  (with-transaction
    (bind ((sister (make-instance 'sister-test))
           (brother (make-instance 'brother-test :sister sister)))
      (is (eq (sister-of brother) sister)))))

(deftest test/association/1-1/initial-value/3 ()
  (with-transaction
    (bind ((brother (make-instance 'brother-test))
           (sister (make-instance 'sister-test :brother brother)))
      (is (eq (brother-of sister) brother)))))

(deftest test/association/1-1/store-value/1 ()
  (with-transaction-for-sister-and-brother
    (setf! (brother-of sister) brother)
    (is (eq brother (brother-of sister)))))

(deftest test/association/1-1/store-value/2 ()
  (with-transaction-for-sister-and-brother
    (setf! (sister-of brother) sister)
    (is (eq sister (sister-of brother)))))

(deftest test/association/1-1/referential-integrity/1 ()
  (with-transaction-for-sister-and-brother
    (setf! (brother-of sister) brother)
    (is (eq sister (sister-of brother)))))

(deftest test/association/1-1/referential-integrity/2 ()
  (with-transaction-for-sister-and-brother
    (setf! (sister-of brother) sister)
    (is (eq brother (brother-of sister)))))

(deftest test/association/1-1/referential-integrity/3 ()
  (with-transaction-for-sister-and-brother
    (setf! (sister-of brother) sister)
    (setf! (sister-of brother) nil)
    (is (eq nil (sister-of brother)))
    (is (eq nil (brother-of sister)))))

(deftest test/association/1-1/referential-integrity/4 ()
  (with-transaction-for-sister-and-brother
    (setf! (brother-of sister) brother)
    (setf! (brother-of sister) nil)
    (is (eq nil (sister-of brother)))
    (is (eq nil (brother-of sister)))))
   
(deftest test/association/1-1/referential-integrity/5 ()
  (with-transaction-for-sister-and-brother
    (setf (sister-of brother) sister)
    (setf (sister-of brother) (make-instance 'sister-test))
    (is (eq nil (brother-of sister)))
    (setf (brother-of sister) brother)
    (setf (brother-of sister) (make-instance 'brother-test))
    (is (eq nil (sister-of brother)))))
