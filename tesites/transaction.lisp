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
  ;; note that it is safe to unconditionally rebind the *t* time machine parameter
  ;; because a new transaction is just being started so we definitely not going to change the
  ;; parameter for an ongoing transaction
  (bind ((*t* (or (t-value-of transaction)
                  ;; TODO: this will always do a select now() but it should be lazy instead
                  (default-t))))
    (call-next-method)))
