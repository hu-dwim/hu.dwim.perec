;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(defclass* transaction-t-mixin ()
  ((t-value nil :initarg :t)))

(defmethod call-in-transaction :around (database (transaction transaction-t-mixin) function)
  (declare (ignore database function))
  ;; TODO: this will always do a select now() but it should be lazy
  (flet ((body ()
           (call-next-method)))
    (aif (t-value-of transaction)
         (with-t it
           (body))
         (with-default-t
           (body)))))
