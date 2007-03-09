;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(defclass* object-cache ()
  ((objects
    (make-hash-table :test #'eq)
    :type hash-table
    :documentation "A map from oid values to persistent objects used to cache object identities and slot values during a transaction.")
   (created-objects
    (make-container 'set-container)
    :type list)
   (modified-objects
    (make-container 'set-container)
    :type list)
   (deleted-objects
    (make-container 'set-container)
    :type list))
  (:documentation "Each transaction has its own transaction level object cache filled by the operations executed during that transaction. The cache is created empty when the transaction starts and it will be dropped when the transaction ends. Each object loaded during a transaction will be put here to keep the identity of the in-memory object throughout the transaction. Moreover the object cache is responsible to manage the list of created, modified and deleted objects during the transaction."))

(defclass* transaction-mixin ()
  ((object-cache
    (make-instance 'object-cache)
    :type object-cache)))

(defun current-object-cache ()
  "Returns the object cache of the current transaction."
  (object-cache-of *transaction*))

(defun cached-object-of (oid &optional (object-cache (current-object-cache)))
  "Returns the object for the given oid from the current transaction's object cachce."
  (gethash (oid-id oid) (objects-of object-cache)))

(defun (setf cached-object-of) (object oid &optional (object-cache (current-object-cache)))
  "Puts an object with the given oid into the current transaction's object cache and attaches it to the current transaction. The object must not be present in the cache before."
  (assert (not (instance-in-transaction-p object)))
  (assert (not (cached-object-of oid object-cache)))
  (setf (transaction-of object) *transaction*)
  (setf (gethash (oid-id oid) (objects-of object-cache)) object))

(defun remove-cached-object (object &optional (object-cache (current-object-cache)))
  "Removes an object from the current transaction's object cache and detaches it from the transaction."
  (assert (instance-in-transaction-p object))
  (assert (cached-object-of (oid-of object) object-cache))
  (setf (transaction-of object) nil)
  (remhash (oid-id (oid-of object)) (objects-of object-cache)))

(defun map-cached-objects (function &optional (object-cache (current-object-cache)))
  "Maps the given one parameter function to all objects present in the cache."
  (maphash #L(funcall function !2) (objects-of object-cache)))

(defun current-objects ()
  "Returns the set of objects in the current transaction."
  (objects-of (current-object-cache)))

(defun current-created-objects ()
  "Returns the set of created objects in the current transaction."
  (created-objects-of (current-object-cache)))

(defun current-modified-objects ()
  "Returns the set of modified objects in the current transaction."
  (modified-objects-of (current-object-cache)))

(defun current-deleted-objects ()
  "Returns the set of deleted objects in the current transaction."
  (deleted-objects-of (current-object-cache)))
