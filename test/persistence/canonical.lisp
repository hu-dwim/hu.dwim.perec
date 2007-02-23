;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

(defsuite* (test/persistence/canonical :in test/persistence))

(deftest test/persistence/canonical/type (type canonical-type)
  (is (equalp canonical-type (prc::canonical-type-for type))))

(defmacro def-canonical-type-test (type canonical-type &optional name)
  `(deftest ,(prc::concatenate-symbol 'test/persistence/canonical "/" (or name type)) ()
    (test/persistence/canonical/type ',type ',canonical-type)))

(def-canonical-type-test null null)

(def-canonical-type-test unbound (member prc::+unbound-slot-value+))

(def-canonical-type-test boolean boolean)

(def-canonical-type-test integer integer)

(def-canonical-type-test float float)

(def-canonical-type-test double double)

(def-canonical-type-test number number)

(def-canonical-type-test (member a b c) (member a b c) member)

(defptype t1 ()
    ()
  '(member a b c))

(def-canonical-type-test
    (or null unbound t1)
    (or null
        (member prc::+unbound-slot-value+)
        (member a b c))
  complex/1)

(def-canonical-type-test
    (and (not null)
         (not unbound)
         (or null unbound t1))
    (member a b c)
  complex/2)

(defptype t2 ()
    ()
  '(or null unbound t1))

(def-canonical-type-test
    (and (not null)
         (not unbound)
         t2)
    (member a b c)
  complex/3)
