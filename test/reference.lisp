;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

(defsuite* test/reference :in test)

(defpclass* referred-test ()
  ())

(defpclass* reference-test ()
  ((referred :type referred-test)))

(defmacro with-reference-transaction (&body body)
  `(with-transaction
    (let ((referred (make-instance 'referred-test))
          ;; TODO: remove this nil
          (reference (make-instance 'reference-test :referred nil)))
      (declare (ignorable referred reference))
      ,@body)))

(deftest test/reference/initial-value ()
  (with-reference-transaction
    (is (eq nil (referred-of reference)))))

(deftest test/reference/store-value/1 ()
  (with-reference-transaction
    (setf (referred-of reference) referred)
    (is (eq referred (referred-of reference)))))
