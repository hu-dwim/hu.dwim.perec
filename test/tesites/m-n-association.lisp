;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

;;;;;;;;;;;;;;;;;;;
;;; m-n association

(defsuite* (test/tesites/association/m-n :in test/tesites/association))

(defpclass* tesites-student-test ()
  ())
   
(defpclass* tesites-course-test ()
  ())

(defassociation*
  ((:class tesites-course-test :slot students :type (set tesites-student-test))
   (:class tesites-student-test :slot courses :type (set tesites-course-test))))

#+nil
(defassociation*
  ((:class tesites-course-test :slot temporal-students :type (set tesites-student-test))
   (:class tesites-student-test :slot temporal-courses :type (set tesites-course-test)))
  (:temporal #t))

#+nil
(defassociation*
  ((:class tesites-course-test :slot time-dependent-students :type (set tesites-student-test))
   (:class tesites-student-test :slot time-dependent-courses :type (set tesites-course-test)))
  (:time-dependent #t))

#+nil
(defassociation*
  ((:class tesites-course-test :slot temporal-and-time-dependent-students :type (set tesites-student-test))
   (:class tesites-student-test :slot temporal-and-time-dependent-courses :type (set tesites-course-test)))
  (:temporal #t)
  (:time-dependent #t))

(deftest test/tesites/association/m-n/normal ()
  (run-complex-test :class-names '(tesites-student-test tesites-course-test)
                    :slot-names '(students courses)
                    :instance-count 10
                    :operation-count 100))

(deftest test/tesites/association/m-n/temporal ()
  (run-complex-test :class-names '(tesites-student-test tesites-course-test)
                    :slot-names '(temporal-students temporal-courses)
                    :instance-count 10
                    :operation-count 100))

(deftest test/tesites/association/m-n/time-dependent ()
  (run-complex-test :class-names '(tesites-student-test tesites-course-test)
                    :slot-names '(time-dependent-students time-dependent-courses)
                    :instance-count 10
                    :operation-count 100))

(deftest test/tesites/association/m-n/temporal-and-time-dependent ()
  (run-complex-test :class-names '(tesites-student-test tesites-course-test)
                    :slot-names '(temporal-and-time-dependent-students temporal-and-time-dependent-courses)
                    :instance-count 10
                    :operation-count 100))
