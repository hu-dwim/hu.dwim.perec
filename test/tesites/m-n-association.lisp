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
  ((:class tesites-course-test :slot parent :type (or null tesites-student-test))
   (:class tesites-student-test :slot children :type (set tesites-course-test)))
  (:time-depenent #t)
  (:temporal #t))
