(in-package :cl-perec-test)

(defsuite* (test/query/select/association/m-n :in test/query/select/association))

(defmacro with-student-and-course-in-transaction (&body body)
  `(with-transaction
    (purge-instances 'course-test)
    (purge-instances 'student-test)
    (bind ((student (make-instance 'student-test))
           (course (make-instance 'course-test)))
      ,@body)))

#+nil(deftest test/query/select/association/m-n/1 ()
  (with-student-and-course-in-transaction
    (is (null (select ((s student-test))
                (assert (member s (students-of course)))
                (collect s))))
    (is (null (select ((c course-test))
                (assert (member c (courses-of student)))
                (collect c))))))

(deftest test/query/select/association/m-n/2 ()
  (with-student-and-course-in-transaction
    (is (equal (select ((s student-test))
                 (assert (null (courses-of s)))
                 (collect s))
               (list student)))
    (is (equal (select ((c course-test))
                 (assert (null (students-of c)))
                 (collect c))
               (list course)))))

(deftest test/query/select/association/m-n/3 ()
  (with-student-and-course-in-transaction
    (setf (students-of course) (list student))
    (is (equal (select ((s student-test))
                 (assert (member s (students-of course)))
                 (collect s))
               (list student)))
    (is (equal (select ((c course-test))
                 (assert (member c (courses-of student)))
                 (collect c))
               (list course)))))

(deftest test/query/select/association/m-n/4 ()
  (with-student-and-course-in-transaction
    (setf (students-of course) (list student))
    (is (null (select ((s student-test))
                (assert (null (courses-of s)))
                (collect s))))
    (is (null (select ((c course-test))
                (assert (null (students-of c)))
                (collect c))))))