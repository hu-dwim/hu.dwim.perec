;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

(defun import-cl-perec-symbols ()
  (import '(cl-perec::primary-table-of
            cl-perec::primary-tables-of
            cl-perec::data-tables-of
            cl-perec::prefetch-p
            cl-perec::cache-p
            cl-perec::table-of
            cl-perec::columns-of
            cl-perec::depends-on-of
            cl-perec::depends-on-me-of)))

(defun drop-all-test-tables ()
  (with-transaction
    (mapc #L(drop-table !1)
          (prc::collect-if #L(and (starts-with !1 "_")
                                  (search "test" !1))
                           (list-tables)))))

(defmacro with-and-without-caching-slot-values (&body forms)
  `(progn
    (without-caching-slot-values
      ,@forms)
    (with-caching-slot-values
      ,@forms)))

(defmacro with-two-transactions (form-1 &body forms-2)
  `(let ((-instance-
          (with-transaction
            ,form-1)))
    (with-transaction
      (revive-instance -instance-)
      ,@forms-2)))

(defmacro with-one-and-two-transactions (form-1 &body forms-2)
  `(progn
    (with-transaction
      (let ((-instance- ,form-1))
        (declare (ignorable -instance-))
        ,@forms-2))
    (with-two-transactions ,form-1 ,@forms-2)))

(defun retest ()
  (drop-all-test-tables)
  (mapc #L(awhen (cl-perec::primary-table-of !1)
            (cl-perec::invalidate-computed-slot it 'prc::ensure-exported))
        (hash-table-values cl-perec::*persistent-classes*))
  (test))

(in-root-suite)

(defsuite* test)

(defsuite* (test/persistence :in test))
