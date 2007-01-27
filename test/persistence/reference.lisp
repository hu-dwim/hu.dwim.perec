;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

(defsuite* test/reference :in test/persistence)

(defpclass* referred-test ()
  ())

(defpclass* reference-test ()
  ((referred :type (or null referred-test))))

(defmacro with-reference-transaction (&body body)
  `(with-transaction
    (let ((referred (make-instance 'referred-test))
          (reference (make-instance 'reference-test)))
      (declare (ignorable referred reference))
      ,@body)))

(deftest test/reference/initial-value ()
  (with-reference-transaction
    (is (eq nil (referred-of reference)))))

(deftest test/reference/store-value/1 ()
  (with-reference-transaction
    (setf (referred-of reference) referred)
    (is (eq referred (referred-of reference)))))
