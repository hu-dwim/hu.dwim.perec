(in-package :cl-perec-test)

(defsuite* (test/persistence/association/m-n :in test/persistence/association)
  (with-and-without-caching-slot-values
    (run-child-tests)))

(defpclass* student-test ()
  ())
   
(defpclass* course-test ()
  ())

(defassociation*
  ((:class student-test :slot courses :type (set course-test))
   (:class course-test :slot students :type (set student-test))))

(defmacro with-student-and-course-transaction (&body body)
  `(with-transaction
    (bind ((student (make-instance 'student-test))
           (course (make-instance 'course-test)))
      ,@body)))

(deftest test/persistence/association/m-n/class ()
  (let ((students-slot (prc::find-slot 'course-test 'students))
        (courses-slot (prc::find-slot 'student-test 'courses)))
    (is (not (prc::primary-table-slot-p students-slot)))
    (is (not (prc::data-table-slot-p students-slot)))
    (is (not (prc::primary-table-slot-p courses-slot)))
    (is (not (prc::data-table-slot-p courses-slot)))))

(deftest test/persistence/association/m-n/initial-value/1 ()
  (with-student-and-course-transaction
    (is (null (courses-of student)))
    (is (null (students-of course)))
    (is (= 0 (size (courses-of* student))))
    (is (= 0 (size (students-of* course))))))

(deftest test/persistence/association/m-n/initial-value/2 ()
  (with-transaction
    (bind ((course (make-instance 'course-test))
           (student (make-instance 'student-test :courses (list course))))
      (is (equal (courses-of student) (list course))))))

(deftest test/persistence/association/m-n/store-value/1 ()
  (with-student-and-course-transaction
    (setf (courses-of student) (list course))
    (is (equal (list course) (courses-of student)))))

(deftest test/persistence/association/m-n/referential-integrity/1 ()
  (with-student-and-course-transaction
    (setf (courses-of student) (list course))
    (bind ((students (students-of* course)))
      (is (= 1 (size students)))
      (is (eq student (first (list-of students)))))))

(deftest test/persistence/association/m-n/collection/1 ()
  (with-student-and-course-transaction
    (bind ((courses (courses-of* student)))
      (insert-item courses course)
      (is (= 1 (size courses)))
      (is (equal (list course) (courses-of student)))
      (delete-item courses course)
      (is (= 0 (size courses)))
      (is (null (courses-of student))))))

(deftest test/persistence/association/m-n/collection/2 ()
  (with-student-and-course-transaction
    (bind ((courses (courses-of* student))
           (other-course (make-instance 'course-test)))
      (insert-item courses course)
      (insert-item courses other-course)
      (delete-item courses course)
      (is (= 1 (size courses)))
      (is (equal (list other-course) (list-of courses))))))
