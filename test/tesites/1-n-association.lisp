;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

;;;;;;;;;;;;;;;;;;;
;;; 1-n association

(defsuite* (test/tesites/association/1-n :in test/tesites))

(defpclass* tesites-parent-test ()
  ())
   
(defpclass* tesites-child-test ()
  ())

(defassociation*
  ((:class tesites-child-test :slot parent :type (or null tesites-parent-test))
   (:class tesites-parent-test :slot children :type (set tesites-child-test))))

(defassociation*
  ((:class tesites-child-test :slot temporal-parent :type (or null tesites-parent-test))
   (:class tesites-parent-test :slot temporal-children :type (set tesites-child-test)))
  (:temporal #t))

(defassociation*
  ((:class tesites-child-test :slot time-dependent-parent :type (or null tesites-parent-test))
   (:class tesites-parent-test :slot time-dependent-children :type (set tesites-child-test)))
  (:time-dependent #t))

(defassociation*
  ((:class tesites-child-test :slot temporal-and-time-dependent-parent :type (or null tesites-parent-test))
   (:class tesites-parent-test :slot temporal-and-time-dependent-children :type (set tesites-child-test)))
  (:temporal #t)
  (:time-dependent #t))

(deftest test/tesites/association/1-n/complex ()
  (run-complex-test :class-names '(tesites-parent-test tesites-child-test)
                    :instance-count 20
                    :repeat-count 10))
