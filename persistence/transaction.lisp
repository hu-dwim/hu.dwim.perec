;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;; with-database, open-database, close-database, with-transaction, with-transaction*, begin, commit, rollback are all inherited from cl-rdbms

(defun transaction-of (object)
  "Returns the transaction to which the object is currently attached to or nil if the object is not known to be part of any ongoing transaction."
  (awhen (slot-value object 'transaction)
    (weak-pointer-value it)))

(defun (setf transaction-of) (transaction object)
  "Attaches the object to a different transaction."
  (assert (or (not transaction)
              (not (transaction-of object))))
  (setf (slot-value object 'transaction)
        (awhen transaction
          (make-weak-pointer transaction))))

(defun instance-in-transaction-p (object)
  "Returns true iff the object is attached to a transaction which is in progress."
  (awhen (transaction-of object)
    (transaction-in-progress-p it)))

(defun instance-in-current-transaction-p (object)
  "Returns true iff the object is attached to the current transaction which is in progress."
  (and (in-transaction-p)
       (eq (transaction-of object) *transaction*)))
