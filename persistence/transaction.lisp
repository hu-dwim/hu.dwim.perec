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

(defmethod rdbms::cleanup-transaction :around ((transaction transaction-mixin))
  (unwind-protect
       (call-next-method)
    (map-cached-instances
     #L(setf (transaction-of !1) nil))))

(defun instance-in-transaction-p (instance)
  "Returns true iff the instance is attached to a transaction which is in progress."
  (not (null (aprog1 (transaction-of instance)
               (debug-only
                 (assert (or (not it) (transaction-in-progress-p it))))))))

(defun instance-in-current-transaction-p (instance)
  "Returns true iff the instance is attached to the current transaction which is in progress."
  (and (in-transaction-p)
       (eq (transaction-of instance) *transaction*)))

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
