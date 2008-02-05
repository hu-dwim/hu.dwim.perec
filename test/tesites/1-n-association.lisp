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
   (:class tesites-parent-test :slot children :type (set tesites-child-test)))
  (:time-dependent #t)
  (:temporal #t))
