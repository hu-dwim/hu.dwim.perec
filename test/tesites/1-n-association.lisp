;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

;;;;;;;;;;;;;;;;;;;
;;; 1-n association

(defsuite* (test/tesites/association/1-n :in test/tesites/association))

(defpclass* tesites-parent-test ()
  ())
   
(defpclass* tesites-child-test ()
  ())

(defassociation*
  ((:class tesites-child-test :slot parent :type (or null tesites-parent-test))
   (:class tesites-parent-test :slot children :type (set tesites-child-test))))

#+nil
(defassociation*
  ((:class tesites-child-test :slot temporal-parent :type (or null tesites-parent-test))
   (:class tesites-parent-test :slot temporal-children :type (set tesites-child-test)))
  (:temporal #t))

#+nil
(defassociation*
  ((:class tesites-child-test :slot time-dependent-parent :type (or null tesites-parent-test))
   (:class tesites-parent-test :slot time-dependent-children :type (set tesites-child-test)))
  (:time-dependent #t))

#+nil
(defassociation*
  ((:class tesites-child-test :slot temporal-and-time-dependent-parent :type (or null tesites-parent-test))
   (:class tesites-parent-test :slot temporal-and-time-dependent-children :type (set tesites-child-test)))
  (:temporal #t)
  (:time-dependent #t))

(deftest test/tesites/association/1-n/normal ()
  (run-complex-tests :class-names '(tesites-parent-test tesites-child-test)
                     :slot-names '(parent children)))

(deftest test/tesites/association/1-n/temporal ()
  (run-complex-tests :class-names '(tesites-parent-test tesites-child-test)
                     :slot-names '(temporal-parent temporal-children)))

(deftest test/tesites/association/1-n/time-dependent ()
  (run-complex-tests :class-names '(tesites-parent-test tesites-child-test)
                     :slot-names '(time-dependent-parent time-dependent-children)))

(deftest test/tesites/association/1-n/temporal-and-time-dependent ()
  (run-complex-tests :class-names '(tesites-parent-test tesites-child-test)
                     :slot-names '(temporal-and-time-dependent-parent temporal-and-time-dependent-children)))
