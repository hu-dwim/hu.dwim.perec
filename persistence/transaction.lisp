;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;; with-database, open-database, close-database, with-transaction, with-transaction*, begin, commit, rollback are all inherited from cl-rdbms

(defclass* transaction-mixin ()
  ((instance-cache
    (make-instance 'instance-cache)
    :type instance-cache)))

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

(defun assert-consistent-event (instance event)
  (assert (and (eq (eq event :created) (created-p instance))
               (eq (eq event :modified) (modified-p instance))
               (eq (eq event :deleted) (deleted-p instance)))))

(defgeneric before-committing-instance (instance event)
  (:method (instance event)
           (values))
  #+debug
  (:method :around (instance event)
           (assert-consistent-event instance event)))

(defgeneric after-instance-committed (instance event)
  (:method (instance event)
           (values))

  #+debug
  (:method :around (instance event)
           (assert-consistent-event instance event)))

(defmethod cl-rdbms::commit-transaction :around (database (transaction transaction-mixin))
  (map-created-instances (rcurry #'before-committing-instance :created))
  (map-modified-instances (rcurry #'before-committing-instance :modified))
  (map-deleted-instances (rcurry #'before-committing-instance :deleted))
  (call-next-method)
  (map-created-instances (rcurry #'after-instance-committed :created))
  (map-modified-instances (rcurry #'after-instance-committed :modified))
  (map-deleted-instances (rcurry #'after-instance-committed :deleted)))
