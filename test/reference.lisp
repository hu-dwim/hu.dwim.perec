;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

(defsuite test/reference)

(in-suite test/reference)

(defpclass* referred ()
  ())

(defpclass* referer ()
  ((referred :type referred)))

(defmacro with-transaction-for-referring-entities (&body body)
  `(with-transaction
    (let ((referred (make-instance 'referred))
          ;; TODO: remove this nil
          (referer (make-instance 'referer :referred nil)))
      (declare (ignorable referred referer))
      ,@body)))

(deftest test/reference/initial-value ()
  (with-transaction-for-referring-entities
    (is (eq nil (referred-of referer)))))

(deftest test/reference/store-value/1 ()
  (with-transaction-for-referring-entities
    (setf (referred-of referer) referred)
    (is (eq referred (referred-of referer)))))
