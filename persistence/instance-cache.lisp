;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(defclass* instance-cache ()
  ((instances
    (make-hash-table :test #'eql)
    :type hash-table
    :documentation "A map from oid values to persistent instances used to cache instance identities and slot values during a transaction.")
   (created-instances
    (make-hash-table :test #'eq)
    :type hash-table
    :documentation "A map from instances to true indicating that the instance is in the set.")
   (modified-instances
    (make-hash-table :test #'eq)
    :type hash-table
    :documentation "A map from instances to true indicating that the instance is in the set.")
   (deleted-instances
    (make-hash-table :test #'eq)
    :type hash-table
    :documentation "A map from instances to true indicating that the instance is in the set."))
  (:documentation "Each transaction has its own transaction level instance cache filled by the operations executed during that transaction. The cache is created empty when the transaction starts and it will be dropped when the transaction ends. Each instance loaded during a transaction will be put here to keep the identity of the in-memory instance throughout the transaction. Moreover the instance cache is responsible to manage the list of created, modified and deleted instances during the transaction."))

(define-symbol-macro *instance-cache* (instance-cache-of *transaction*))

(def (function io) cached-instance-of (oid)
  "Returns the instance for the given oid from the current transaction's instance cachce."
  (gethash (oid-instance-id oid) (instances-of *instance-cache*)))

(def (function io) (setf cached-instance-of) (instance oid)
  "Puts an instance with the given oid into the current transaction's instance cache and attaches it to the current transaction. The instance must not be present in the cache before."
  (debug-only
    (assert (not (instance-in-transaction-p instance)))
    (assert (not (cached-instance-of oid))))
  (setf (transaction-of instance) *transaction*)
  (let ((key (oid-instance-id oid)))
    (assert key)
    (setf (gethash key (instances-of *instance-cache*)) instance)))

(def (function io) remove-cached-instance (instance)
  "Removes an instance from the current transaction's instance cache and detaches it from the transaction."
  (bind ((oid (oid-of instance))
         (key (oid-instance-id oid)))
    (debug-only
      (assert (instance-in-transaction-p instance))
      (assert (cached-instance-of oid)))
    (setf (transaction-of instance) nil)
    (remhash key (instances-of *instance-cache*))))

(def (function io) map-cached-instances (function)
  "Maps the given one parameter function to all instances present in the cache."
  (declare (type function function))
  (alexandria:maphash-values function (instances-of *instance-cache*)))

(def (function io) map-created-instances (function)
  (declare (type function function))
  (alexandria:maphash-keys function (created-instances-of *instance-cache*)))

(def (function io) map-modified-instances (function)
  (declare (type function function))
  (alexandria:maphash-keys function (modified-instances-of *instance-cache*)))

(def (function io) map-deleted-instances (function)
  (declare (type function function))
  (alexandria:maphash-keys function (deleted-instances-of *instance-cache*)))

(defun update-instance-cache-for-created-instance (instance)
  (ecase (event-of instance)
    (:created)
    ((:modified :deleted)
     (error "Inconsistent cache"))
    ((nil)
     (setf (event-of instance) :created)
     (setf (gethash instance (created-instances-of *instance-cache*)) #t))))

(defun update-instance-cache-for-modified-instance (instance)
  (unless (event-of instance)
    (setf (event-of instance) :modified)
    (setf (gethash instance (modified-instances-of *instance-cache*)) #t)))

(defun update-instance-cache-for-deleted-instance (instance)
  (ecase (event-of instance)
    (:created
     (setf (event-of instance) nil)
     (remhash instance (created-instances-of *instance-cache*)))
    (:modified
     (setf (event-of instance) :deleted)
     (remhash instance (modified-instances-of *instance-cache*))
     (setf (gethash instance (deleted-instances-of *instance-cache*)) #t))
    (:deleted)
    ((nil)
     (setf (event-of instance) :deleted)
     (setf (gethash instance (deleted-instances-of *instance-cache*)) #t))))
