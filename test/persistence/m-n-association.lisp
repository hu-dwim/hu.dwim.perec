(in-package :cl-perec-test)

(defvar *association-m-n-student-class-name* 'student-test)

(defvar *association-m-n-course-class-name* 'course-test)

(defsuite* (test/persistence/association/m-n :in test/persistence/association) ()
  (flet ((body ()
           (with-and-without-caching-slot-values
             (run-child-tests))))
    (body)
    (bind ((*association-m-n-student-class-name* 'm-n-self-association-test)
           (*association-m-n-course-class-name* 'm-n-self-association-test))
      (body))
    (bind ((*association-m-n-student-class-name* 'concrete-student-test)
           (*association-m-n-course-class-name* 'concrete-course-test))
      (body))))

(defpclass* student-test ()
  ())
   
(defpclass* course-test ()
  ())

(defassociation*
  ((:class student-test :slot courses :type (set course-test))
   (:class course-test :slot students :type (set student-test))))

(defpclass* m-n-self-association-test ()
  ())

(defassociation*
  ((:class m-n-self-association-test :slot students :type (set m-n-self-association-test))
   (:class m-n-self-association-test :slot courses :type (set m-n-self-association-test))))

(defpclass* abstract-student-test ()
  ()
  (:abstract #t)
  (:direct-store :push-down))

(defpclass* concrete-student-test (abstract-student-test)
  ())
   
(defpclass* abstract-course-test ()
  ()
  (:abstract #t)
  (:direct-store :push-down))

(defpclass* concrete-course-test (abstract-course-test)
  ())

(defassociation*
  ((:class abstract-student-test :slot courses :type (set abstract-course-test))
   (:class abstract-course-test :slot students :type (set abstract-student-test))))

(defmacro with-student-and-course-transaction (&body body)
  `(with-transaction
     (bind ((student (make-instance *association-m-n-student-class-name*))
            (course (make-instance *association-m-n-course-class-name*)))
       ,@body)))

(deftest test/persistence/association/m-n/class ()
  (ensure-exported (find-class *association-m-n-course-class-name*))
  (ensure-exported (find-class *association-m-n-student-class-name*))
  (let ((students-slot (find-slot *association-m-n-course-class-name* 'students))
        (courses-slot (find-slot *association-m-n-student-class-name* 'courses)))
    (is (not (primary-table-slot-p students-slot)))
    (is (not (data-table-slot-p students-slot)))
    (is (not (primary-table-slot-p courses-slot)))
    (is (not (data-table-slot-p courses-slot)))
    (is (not (cache-p students-slot)))
    (is (not (cache-p courses-slot)))))

(deftest test/persistence/association/m-n/initial-value/1 ()
  (with-student-and-course-transaction
    (is (null (courses-of student)))
    (is (null (students-of course)))
    (is (= 0 (size (courses-of* student))))
    (is (= 0 (size (students-of* course))))))

(deftest test/persistence/association/m-n/initial-value/2 ()
  (with-transaction
    (bind ((course (make-instance *association-m-n-course-class-name*))
           (student (make-instance *association-m-n-student-class-name* :courses (list course))))
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
    (bind ((courses (courses-of* student)))
      (insert-item courses course)
      (ensure-item courses course)
      (signals error (insert-item courses course)))))

(deftest test/persistence/association/m-n/collection/3 ()
  (with-student-and-course-transaction
    (bind ((courses (courses-of* student))
           (other-course (make-instance *association-m-n-course-class-name*)))
      (insert-item courses course)
      (insert-item courses other-course)
      (delete-item courses course)
      (is (= 1 (size courses)))
      (is (equal (list other-course) (list-of courses))))))
