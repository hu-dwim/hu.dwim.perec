;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(defclass* instance-cache ()
  ((instances
    (make-hash-table :test #'eq)
    :type hash-table
    :documentation "A map from oid values to persistent instances used to cache instance identities and slot values during a transaction.")
   (created-instances
    (make-container 'set-container)
    :type list)
   (modified-instances
    (make-container 'set-container)
    :type list)
   (deleted-instances
    (make-container 'set-container)
    :type list))
  (:documentation "Each transaction has its own transaction level instance cache filled by the operations executed during that transaction. The cache is created empty when the transaction starts and it will be dropped when the transaction ends. Each instance loaded during a transaction will be put here to keep the identity of the in-memory instance throughout the transaction. Moreover the instance cache is responsible to manage the list of created, modified and deleted instances during the transaction."))

(defun current-instance-cache ()
  "Returns the instance cache of the current transaction."
  (instance-cache-of *transaction*))

(defun cached-instance-of (oid &optional (instance-cache (current-instance-cache)))
  "Returns the instance for the given oid from the current transaction's instance cachce."
  (gethash (oid-instance-id oid) (instances-of instance-cache)))

(defun (setf cached-instance-of) (instance oid &optional (instance-cache (current-instance-cache)))
  "Puts an instance with the given oid into the current transaction's instance cache and attaches it to the current transaction. The instance must not be present in the cache before."
  (assert (not (instance-in-transaction-p instance)))
  (assert (not (cached-instance-of oid instance-cache)))
  (setf (transaction-of instance) *transaction*)
  (setf (gethash (oid-instance-id oid) (instances-of instance-cache)) instance))

(defun remove-cached-instance (instance &optional (instance-cache (current-instance-cache)))
  "Removes an instance from the current transaction's instance cache and detaches it from the transaction."
  (assert (instance-in-transaction-p instance))
  (assert (cached-instance-of (oid-of instance) instance-cache))
  (setf (transaction-of instance) nil)
  (remhash (oid-instance-id (oid-of instance)) (instances-of instance-cache)))

(defun map-cached-instances (function &optional (instance-cache (current-instance-cache)))
  "Maps the given one parameter function to all instances present in the cache."
  (maphash #L(funcall function !2) (instances-of instance-cache)))

(defun map-created-instances (function &optional (instance-cache (current-instance-cache)))
  (iterate-nodes (created-instances-of instance-cache) function))

(defun map-modified-instances (function &optional (instance-cache (current-instance-cache)))
  (iterate-nodes (modified-instances-of instance-cache) function))

(defun map-deleted-instances (function &optional (instance-cache (current-instance-cache)))
  (iterate-nodes (deleted-instances-of instance-cache) function))

(defun update-cache-for-created-instance (instance &optional (instance-cache (current-instance-cache)))
  (cond ((created-p instance))
        ((or (modified-p instance)
             (deleted-p instance))
         (error "Inconsistent cache"))
        (t
         (setf (created-p instance) #t)
         (insert-item (created-instances-of instance-cache) instance))))

(defun update-cache-for-modified-instance (instance &optional (instance-cache (current-instance-cache)))
  (when (and (not (created-p instance))
             (not (modified-p instance))
             (not (deleted-p instance)))
    (setf (modified-p instance) #t)
    (insert-item (modified-instances-of instance-cache) instance)))

(defun update-cache-for-deleted-instance (instance &optional (instance-cache (current-instance-cache)))
  (cond ((created-p instance)
         (setf (created-p instance) #f)
         (delete-item (created-instances-of instance-cache) instance))
        ((modified-p instance)
         (setf (modified-p instance) #f)
         (delete-item (modified-instances-of instance-cache) instance)
         (setf (deleted-p instance) #t)
         (insert-item (deleted-instances-of instance-cache) instance))
        ((deleted-p instance))
        (t
         (setf (deleted-p instance) #t)
         (insert-item (deleted-instances-of instance-cache) instance))))
