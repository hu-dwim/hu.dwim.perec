;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Persistent object base class

(defvar *make-persistent-instances* #t
  "True means make-instance will make the new instance persistent by default.")

(defpclass* persistent-object ()
  ((oid
    nil
    :type (or null oid)
    :persistent #f
    :documentation "Life time unique identifier of the instance which can be remembered and may be used the load the instance later.")
   (persistent
    :type boolean
    :persistent #f
    :documentation "True means the instance is known to be persistent, false means the instance is known to be transient, unbound means the state is not yet determined. Actually, in the latter case slot-value-using-class will automatically determine whether the instance is in the database or not. Therefore reading the persistent slot will always return either true or false.")
   (transaction
    nil
    :type t
    :persistent #f
    :documentation "A reference to the transaction to which this instance is currently attached to or nil.")
   (transaction-event
    nil
    :type (member nil :created :modified :deleted)
    :persistent #f
    :documentation ":created means the instance was created in the current transaction, :modified means the instance was not created but modified in the current transaction. :deleted means the instance was already present at the very beginning but got deleted in the current transaction."))
  (:default-initargs :persistent *make-persistent-instances*)
  (:abstract #t)
  (:documentation "Base class for all persistent classes. If this class is not inherited by a persistent class then it is automatically added to the direct superclasses. There is only one persistent instance in a transaction with a give oid therefore eq will return true iff the oids are equal."))

(defmacro with-making-persistent-instances (&body forms)
  `(let ((*make-persistent-instances* #t))
    ,@forms))

(defmacro with-making-transient-instances (&body forms)
  `(let ((*make-persistent-instances* #f))
    ,@forms))

;;;;;;;;;;;;;;;
;;; MOP methods

(defmethod initialize-instance :around ((instance persistent-object) &rest args &key persistent &allow-other-keys)
  (bind ((class (class-of instance)))
    (when persistent
      (ensure-exported class))
    (iter (for slot :in (effective-slots-with-underlying-slot-access-of class))
          (underlying-slot-makunbound-using-class class instance slot))
    (prog1 (apply #'call-next-method instance :persistent #f args)
      (when (eq persistent #t)
        (make-persistent instance)))))

(defmethod make-instance :before ((class persistent-class) &key &allow-other-keys)
  (when (abstract-p class)
    (error "Cannot make instances of abstract class ~A" class)))

;;;;;;;;;;;
;;; Utility

(defvar +persistent-object-class+ (find-class 'persistent-object))

(defun persistent-object-p (instance)
  (typep instance 'persistent-object))

(defun p-eq (instance-1 instance-2)
  "Tests if two instances are the same persistent instance. Normally there at most one persistent instance for each oid in a transaction so eq may be safely used. On the other hand huge transactions may require to throw away instances form the instance cache which results in several instances for the same oid within the same transaction."
  (or (eq instance-1 instance-2)
      (= (id-of instance-1)
         (id-of instance-2))))

(defun print-persistent-instance (instance)
  (declare (type persistent-object instance))
  (write-string ":persistent ")
  (write-string (cond ((not (slot-boundp instance 'persistent))
                       "#? ")
                      ((persistent-p instance)
                       "#t ")
                      (t "#f ")))
  (if (slot-boundp instance 'oid)
      (aif (oid-of instance)
           (princ (oid-instance-id it))
           (write-string "nil"))
      (write-string "?")))

(defprint-object (self persistent-object)
  "Prints the oid of the instance and whether the instance is known to be persistent or transient."
  (print-persistent-instance self))

(defun created-p (instance)
  (eq :created (transaction-event-of instance)))

(defun modified-p (instance)
  (eq :modified (transaction-event-of instance)))

(defun deleted-p (instance)
  (eq :deleted (transaction-event-of instance)))

(defun ensure-oid (instance)
  "Makes sure that the instance has a valid oid."
  (unless (oid-of instance)
    (setf (oid-of instance) (make-class-oid (class-name (class-of instance))))))

(defun id-of (instance)
  "Shortcut for the unique identifier number of the instance."
  (oid-id (oid-of instance)))
