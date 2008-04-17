;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

;;;;;;;;
;;; Slot

(defsuite* (test/tesites/slot :in test/tesites))

(defpclass* tesites-complex-test ()
  ((slot 0 :type integer-32)
   (temporal-slot 0 :type integer-32 :temporal #t)
   (time-dependent-slot 0 :type integer-32 :time-dependent #t)
   (temporal-and-time-dependent-slot 0 :type integer-32 :temporal #t :time-dependent #t)))

(defpclass* tesites-complex-unbound-test ()
  ((slot :type (or unbound integer-32))
   (temporal-slot :type (or unbound integer-32) :temporal #t)
   (time-dependent-slot :type (or unbound integer-32) :time-dependent #t)
   (temporal-and-time-dependent-slot :type (or unbound integer-32) :temporal #t :time-dependent #t)))

(defpclass* tesites-complex-null-test ()
  ((slot :type (or null integer-32))
   (temporal-slot :type (or null integer-32) :temporal #t)
   (time-dependent-slot :type (or null integer-32) :time-dependent #t)
   (temporal-and-time-dependent-slot :type (or null integer-32) :temporal #t :time-dependent #t)))

(defpclass* tesites-complex-unbound-or-null-test ()
  ((slot :type (or unbound null integer-32))
   (temporal-slot :type (or unbound null integer-32) :temporal #t)
   (time-dependent-slot :type (or unbound null integer-32) :time-dependent #t)
   (temporal-and-time-dependent-slot :type (or unbound null integer-32) :temporal #t :time-dependent #t)))

(defpclass* tesites-complex-slot-test ()
  ((slot :type (or null integer-32))))

(defpclass* tesites-complex-temporal-slot-test ()
  ((temporal-slot :type (or null integer-32) :temporal #t)))

(defpclass* tesites-complex-time-dependent-slot-test ()
  ((time-dependent-slot :type (or null integer-32) :time-dependent #t)))

(defpclass* tesites-complex-temporal-and-time-dependent-slot-test ()
  ((temporal-and-time-dependent-slot :type (or null integer-32) :temporal #t :time-dependent #t)))

(defpclass* tesites-complex-inheritance-test
    (tesites-complex-slot-test tesites-complex-temporal-slot-test tesites-complex-time-dependent-slot-test tesites-complex-temporal-and-time-dependent-slot-test)
  ()
  (:metaclass persistent-class-t))

(deftest test/tesites/slot/normal ()
  (run-complex-test :class-name 'tesites-complex-test
                    :instance-count 1
                    :operation-count 10))

(deftest test/tesites/slot/unbound ()
  (run-complex-test :class-name 'tesites-complex-unbound-test
                    :instance-count 1
                    :operation-count 10))

(deftest test/tesites/slot/null ()
  (run-complex-test :class-name 'tesites-complex-null-test
                    :instance-count 1
                    :operation-count 10))

(deftest test/tesites/slot/unbound-or-null ()
  (run-complex-test :class-name 'tesites-complex-unbound-or-null-test
                    :instance-count 1
                    :operation-count 10))

(deftest test/tesites/slot/inheritance ()
  (run-complex-test :class-name 'tesites-complex-inheritance-test
                    :instance-count 1
                    :operation-count 10))
