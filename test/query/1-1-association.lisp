(in-package :cl-perec-test)

(defsuite* (test/query/select/association/1-1 :in test/query/select/association))

(defmacro with-sister-and-brother-in-transaction (&body body)
  `(with-transaction
    (purge-instances 'brother-test)
    (purge-instances 'sister-test)
    (bind ((sister (make-instance 'sister-test))
           (brother (make-instance 'brother-test)))
      ,@body)))

(deftest test/query/select/association/1-1/1 ()
  (with-sister-and-brother-in-transaction
      (is (null (select (s)
                  (from (s sister-test))
                  (where (eq s (sister-of brother))))))
      (is (null (select (b)
                  (from (b brother-test))
                  (where (eq b (brother-of sister))))))))

(deftest test/query/select/association/1-1/2 ()
  (with-sister-and-brother-in-transaction
      (is (equal (select (s)
                   (from (s sister-test))
                   (where (null (brother-of s))))
                 (list sister)))
      (is (equal (select (b)
                   (from (b brother-test))
                   (where (null (sister-of b))))
                 (list brother)))))

(deftest test/query/select/association/1-1/3 ()
  (with-sister-and-brother-in-transaction
    (setf (sister-of brother) sister)
    (is (equal (select (s)
                 (from (s sister-test))
                 (where (eq s (sister-of brother))))
               (list sister)))
    (is (equal (select (b)
                 (from (b brother-test))
                 (where (eq b (brother-of sister))))
               (list brother)))))

(deftest test/query/select/association/1-1/4 ()
  (with-sister-and-brother-in-transaction
    (setf (sister-of brother) sister)
    (is (null (select (s)
                (from (s sister-test))
                (where (null (brother-of s))))))
    (is (null (select (b)
                (from (b brother-test))
                (where (null (sister-of b))))))))