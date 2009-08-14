;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(defsuite* (test/query/select/association/m-n :in test/query/select/association))

(defmacro with-student-and-course-in-transaction (&body body)
  `(with-transaction
    (purge-instances 'course-test)
    (purge-instances 'student-test)
    (bind ((student (make-instance 'student-test))
           (course (make-instance 'course-test)))
      ,@body)))

(deftest test/query/select/association/m-n/1 ()
  (with-student-and-course-in-transaction
    (is (null (select (s)
                (from (s student-test))
                (where (member s (students-of course))))))
    (is (null (select (c)
                (from (c course-test))
                (where (member c (courses-of student))))))))

(deftest test/query/select/association/m-n/2 ()
  (with-student-and-course-in-transaction
    (is (equal (select (s)
                 (from (s student-test))
                 (where (null (courses-of s))))
               (list student)))
    (is (equal (select (c)
                 (from (c course-test))
                 (where (null (students-of c))))
               (list course)))))

(deftest test/query/select/association/m-n/3 ()
  (with-student-and-course-in-transaction
    (setf (students-of course) (list student))
    (is (equal (select (s)
                 (from (s student-test))
                 (where (member s (students-of course))))
               (list student)))
    (is (equal (select (c)
                 (from (c course-test))
                 (where (member c (courses-of student))))
               (list course)))))

(deftest test/query/select/association/m-n/4 ()
  (with-student-and-course-in-transaction
    (setf (students-of course) (list student))
    (is (null (select (s)
                (from (s student-test))
                (where (null (courses-of s))))))
    (is (null (select (c)
                (from (c course-test))
                (where (null (students-of c))))))))