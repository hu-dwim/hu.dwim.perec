;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

;;;;;;;;;;;;;;;;;;;
;;; 1-1 association

(defsuite* (test/tesites/association/1-1 :in test/tesites))

(defpclass* tesites-brother-test ()
  ())
   
(defpclass* tesites-sister-test ()
  ())

(defassociation*
  ((:class tesites-sister-test :slot parent :type (or null tesites-brother-test))
   (:class tesites-brother-test :slot children :type (set tesites-sister-test)))
  (:time-dependent #t)
  (:temporal #t))
