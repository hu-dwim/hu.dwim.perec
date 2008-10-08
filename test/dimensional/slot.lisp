;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

;;;;;;;;
;;; Slot

(defsuite* (test/dimensional/slot :in test/dimensional))

(defpclass* dimensional-complex-test ()
  ((slot 0 :type integer-32)
   (time-dependent-slot 0 :type integer-32 :dimensions (time))
   (validity-dependent-slot 0 :type integer-32 :dimensions (validity))
   (time-and-validity-dependent-slot 0 :type integer-32 :dimensions (time validity))))

(defpclass* dimensional-complex-unbound-test ()
  ((slot :type (or unbound integer-32))
   (time-dependent-slot :type (or unbound integer-32) :dimensions (time))
   (validity-dependent-slot :type (or unbound integer-32) :dimensions (validity))
   (time-and-validity-dependent-slot :type (or unbound integer-32) :dimensions (time validity))))

(defpclass* dimensional-complex-null-test ()
  ((slot :type (or null integer-32))
   (time-dependent-slot :type (or null integer-32) :dimensions (time))
   (validity-dependent-slot :type (or null integer-32) :dimensions (validity))
   (time-and-validity-dependent-slot :type (or null integer-32) :dimensions (time validity))))

(defpclass* dimensional-complex-unbound-or-null-test ()
  ((slot :type (or unbound null integer-32))
   (time-dependent-slot :type (or unbound null integer-32) :dimensions (validity))
   (validity-dependent-slot :type (or unbound null integer-32) :dimensions (validity))
   (time-and-validity-dependent-slot :type (or unbound null integer-32) :dimensions (time validity))))

(defpclass* dimensional-complex-slot-test ()
  ((slot :type (or null integer-32))))

(defpclass* dimensional-complex-time-dependent-slot-test ()
  ((time-dependent-slot :type (or null integer-32) :dimensions (time))))

(defpclass* dimensional-complex-validity-dependent-slot-test ()
  ((validity-dependent-slot :type (or null integer-32) :dimensions (validity))))

(defpclass* dimensional-complex-time-and-validity-dependent-slot-test ()
  ((time-and-validity-dependent-slot :type (or null integer-32) :dimensions (time validity))))

(defpclass* dimensional-complex-inheritance-test
    (dimensional-complex-slot-test dimensional-complex-time-dependent-slot-test dimensional-complex-validity-dependent-slot-test dimensional-complex-time-and-validity-dependent-slot-test)
  ()
  (:metaclass persistent-class-d))

(def test test/dimensional/slot/normal ()
  (run-complex-tests :class-name 'dimensional-complex-test
                     :instance-count 1))

(def test test/dimensional/slot/unbound ()
  (run-complex-tests :class-name 'dimensional-complex-unbound-test
                     :instance-count 1))

(def test test/dimensional/slot/null ()
  (run-complex-tests :class-name 'dimensional-complex-null-test
                     :instance-count 1))

(def test test/dimensional/slot/unbound-or-null ()
  (run-complex-tests :class-name 'dimensional-complex-unbound-or-null-test
                     :instance-count 1))

(def test test/dimensional/slot/inheritance ()
  (run-complex-tests :class-name 'dimensional-complex-inheritance-test
                     :instance-count 1))
