;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

;;;;;;;;;;;;;
;;; Canonical

(defsuite* (test/persistence/canonical :in test/persistence))

(defun check-mapped-type (type)
  (is (or (eq type 'unbound)
          (eq type 'member)
          (eq type t)
          (not (prc::unbound-subtype-p type))))
  (is (or (eq type 'set)
          (eq type 'disjunct-set)
          (eq type 'ordered-set)
          (eq type 'member)
          (eq type t)
          (not (prc::set-type-p type)))))

(deftest test/persistence/canonical/subtypep ()
  (mapc #'check-mapped-type prc::*mapped-type-precedence-list*))

(deftest test/persistence/canonical/type (type canonical-type)
  (let ((prc::*canonical-types* '(unbound)))
    (is (equalp canonical-type (prc::canonical-type-for type)))))

(defmacro def-canonical-type-test (name type canonical-type)
  `(deftest ,(prc::concatenate-symbol 'test/persistence/canonical/ name) ()
    (test/persistence/canonical/type ',type ',canonical-type)))

(def-canonical-type-test null/1 null null)

(def-canonical-type-test unbound/1 unbound unbound)

(def-canonical-type-test boolean/1 boolean boolean)
(def-canonical-type-test boolean/2 (and (not unbound) boolean) boolean)

(def-canonical-type-test integer/1 integer integer)
(def-canonical-type-test integer/2 (and (not unbound) (not null) integer) integer)

(def-canonical-type-test float/1 float float)
(def-canonical-type-test float/2 (and (not unbound) (not null) float) float)

(def-canonical-type-test double/1 double double)
(def-canonical-type-test double/2 (and (not unbound) (not null) double) double)

(def-canonical-type-test number/1 number number)
(def-canonical-type-test number/2 (and (not unbound) (not null) number) number)

(def-canonical-type-test string/1 string string)
(def-canonical-type-test string/2 (and (not unbound) (not null) string) string)

(def-canonical-type-test symbol/1 symbol symbol)
(def-canonical-type-test symbol/2 (and (not unbound) symbol) symbol)

(def-canonical-type-test member/1 (member a b c) (member a b c))

(defptype t1 ()
    ()
  '(member a b c))

(def-canonical-type-test complex/1
  (or null unbound t1)
  (or null
      unbound
      (member a b c)))

(def-canonical-type-test complex/2
  (and (not null)
       (not unbound)
       (or null unbound t1))
  (member a b c))

(defptype t2 ()
    ()
  '(or null unbound t1))

(def-canonical-type-test complex/3
  (and (not null)
       (not unbound)
       t2)
  (member a b c))

;;;;;;;;;;;;;;
;;; Normalized

(defsuite* (test/persistence/normalized :in test/persistence))

(deftest test/persistence/normalized/type (type normalized-type)
  (is (equalp normalized-type (prc::normalized-type-for type))))

(defmacro def-normalized-type-test (name type normalized-type)
  `(deftest ,(prc::concatenate-symbol 'test/persistence/normalized/ name) ()
    (test/persistence/normalized/type ',type ',normalized-type)))

(def-normalized-type-test null/1 unbound nil)

(def-normalized-type-test unbound/1 unbound nil)

(def-normalized-type-test boolean/1 boolean (and (not null) boolean))
(def-normalized-type-test boolean/2 (or unbound boolean) (and (not null) boolean))

(def-normalized-type-test integer/1 integer integer)
(def-normalized-type-test integer/2 (or unbound integer) integer)
(def-normalized-type-test integer/3 (or null integer) integer)
(def-normalized-type-test integer/4 (or unbound null integer) integer)

(def-normalized-type-test float/1 float float)
(def-normalized-type-test float/2 (or unbound float) float)
(def-normalized-type-test float/3 (or null float) float)
(def-normalized-type-test float/4 (or unbound null float) float)

(def-normalized-type-test double/1 double double)
(def-normalized-type-test double/2 (or unbound double) double)
(def-normalized-type-test double/3 (or null double) double)
(def-normalized-type-test double/4 (or unbound null double) double)

(def-normalized-type-test number/1 number number)
(def-normalized-type-test number/2 (or unbound number) number)
(def-normalized-type-test number/3 (or null number) number)
(def-normalized-type-test number/4 (or unbound null number) number)

(def-normalized-type-test string/1 string string)
(def-normalized-type-test string/2 (or unbound string) string)
(def-normalized-type-test string/3 (or null string) string)
(def-normalized-type-test string/4 (or unbound null string) string)

(def-normalized-type-test symbol/1 symbol (and (not null) symbol))
(def-normalized-type-test symbol/2 (or unbound symbol) (and (not null) symbol))

(def-normalized-type-test set/1 (set persistent-object) (and (not null) (set persistent-object)))

;;;;;;;;;;;;;;;
;;;; Reflection

(defsuite* (test/persistence/type-reflection :in test/persistence))

(deftest test/persistence/type-reflection/unbound ()
  (let ((type (find-type 'unbound)))
    (is (typep type 'unbound-type))
    (is (subtypep 'unbound-type 'eql-type))))

(deftest test/persistence/type-reflection/null ()
  (let ((type (find-type 'null)))
    (is (typep type 'null-type))
    (is (subtypep 'null-type 'persistent-type))))

(deftest test/persistence/type-reflection/boolean ()
  (let ((type (find-type 'boolean)))
    (is (typep type 'boolean-type))
    (is (subtypep 'boolean-type 'persistent-type))))

(deftest test/persistence/type-reflection/integer-16 ()
  (let ((type (find-type 'integer-16)))
    (is (typep type 'integer-16-type))
    (is (subtypep 'integer-16-type 'integer-type))))

(deftest test/persistence/type-reflection/integer-32 ()
  (let ((type (find-type 'integer-32)))
    (is (typep type 'integer-32-type))
    (is (subtypep 'integer-32-type 'integer-type))))

(deftest test/persistence/type-reflection/integer-64 ()
  (let ((type (find-type 'integer-64)))
    (is (typep type 'integer-64-type))
    (is (subtypep 'integer-64-type 'integer-type))))

(deftest test/persistence/type-reflection/string ()
  (let ((type (find-type 'string)))
    (is (typep type 'string-type))
    (is (subtypep 'string-type 'persistent-type))))

(deftest test/persistence/type-reflection/text ()
  (let ((type (find-type 'text)))
    (is (typep type 'text-type))
    (is (subtypep 'text-type 'string-type))))

(defptype member-test ()
  '(member a b c))

(deftest test/persistence/type-reflection/member ()
  (let ((type (find-type 'member-test)))
    (is (typep type 'member-test-type))
    (is (subtypep 'member-test-type 'member-type))
    (is (equal (members-of type)
               '(a b c)))))

(defptype integer-test ()
  'integer-32)

(deftest test/persistence/type-reflection/integer ()
  (let ((type (find-type 'integer-test)))
    (is (typep type 'integer-test-type))
    (is (subtypep 'integer-test-type 'integer-32-type))))
