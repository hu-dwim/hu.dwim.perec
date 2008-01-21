;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(defclass transaction-t-mixin ()
  ())

;; TODO: disallow changing the t parameter in a transaction
(defmethod call-in-transaction :around (database (transaction transaction-t-mixin) function)
  (declare (ignore database function))
  ;; TODO: this will always do a select now() but it should be lazy
  (with-default-t
    (call-next-method)))
