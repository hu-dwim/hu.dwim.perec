(in-package :cl-perec-test)

(defsuite* (test/query/select/association/1-1 :in test/query/select/association))

(defmacro with-sister-and-brother-in-transaction (&body body)
  `(with-transaction
    (purge-objects 'brother-test)
    (purge-objects 'sister-test)
    (bind ((sister (make-instance 'sister-test))
           (brother (make-instance 'brother-test)))
      ,@body)))

(deftest test/query/select/association/1-1/1 ()
  (with-sister-and-brother-in-transaction
      (is (null (select ((s sister-test))
                  (assert (eq s (sister-of brother)))
                  (collect s))))
      (is (null (select ((b brother-test))
                  (assert (eq b (brother-of sister)))
                  (collect b))))))

(deftest test/query/select/association/1-1/2 ()
  (with-sister-and-brother-in-transaction
      (is (equal (select ((s sister-test))
                   (assert (null (brother-of s)))
                   (collect s))
                 (list sister)))
      (is (equal (select ((b brother-test))
                   (assert (null (sister-of b)))
                   (collect b))
                 (list brother)))))

(deftest test/query/select/association/1-1/3 ()
  (with-sister-and-brother-in-transaction
    (setf (sister-of brother) sister)
    (is (equal (select ((s sister-test))
                 (assert (eq s (sister-of brother)))
                 (collect s))
               (list sister)))
    (is (equal (select ((b brother-test))
                 (assert (eq b (brother-of sister)))
                 (collect b))
               (list brother)))))

(deftest test/query/select/association/1-1/4 ()
  (with-sister-and-brother-in-transaction
    (setf (sister-of brother) sister)
    (is (null (select ((s sister-test))
                (assert (null (brother-of s)))
                (collect s))))
    (is (null (select ((b brother-test))
                (assert (null (sister-of b)))
                (collect b))))))