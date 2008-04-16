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
  ((:class tesites-course-test :slot courses :type (or null tesites-student-test))
   (:class tesites-student-test :slot students :type (set tesites-course-test)))
  (:time-dependent #t)
  (:temporal #t))
