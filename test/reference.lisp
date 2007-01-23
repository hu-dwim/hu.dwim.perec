;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

(in-suite test)

(defsuite test/reference)

(in-suite* test/reference)

(defpclass* referred ()
  ())

(defpclass* reference-test ()
  ((referred :type referred)))

(defmacro with-transaction-for-reference (&body body)
  `(with-transaction
    (let ((referred (make-instance 'referred))
          ;; TODO: remove this nil
          (reference (make-instance 'reference-test :referred nil)))
      (declare (ignorable referred reference))
      ,@body)))

(deftest test/reference/initial-value ()
  (with-transaction-for-reference
    (is (eq nil (referred-of reference)))))

(deftest test/reference/store-value/1 ()
  (with-transaction-for-reference
    (setf (referred-of reference) referred)
    (is (eq referred (referred-of reference)))))
