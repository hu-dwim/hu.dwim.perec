;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

;;;;;;;;;;;;;;;;;;;
;;; m-n association

(defsuite* (test/tesites/association/m-n :in test/tesites))

(defpclass* tesites-student-test ()
  ())
   
(defpclass* tesites-course-test ()
  ())

(defassociation*
  ((:class tesites-course-test :slot courses :type (set tesites-student-test))
   (:class tesites-student-test :slot students :type (set tesites-course-test))))

(defassociation*
  ((:class tesites-course-test :slot temporal-courses :type (set tesites-student-test))
   (:class tesites-student-test :slot temporal-students :type (set tesites-course-test)))
  (:temporal #t))

(defassociation*
  ((:class tesites-course-test :slot time-dependent-courses :type (set tesites-student-test))
   (:class tesites-student-test :slot time-dependent-students :type (set tesites-course-test)))
  (:time-dependent #t))

(defassociation*
  ((:class tesites-course-test :slot temporal-and-time-dependent-courses :type (set tesites-student-test))
   (:class tesites-student-test :slot temporal-and-time-dependent-students :type (set tesites-course-test)))
  (:temporal #t)
  (:time-dependent #t))

(deftest test/tesites/association/m-n/complex ()
  (run-complex-test :class-names '(tesites-student-test tesites-course-test)
                    :instance-count 20
                    :repeat-count 10))
