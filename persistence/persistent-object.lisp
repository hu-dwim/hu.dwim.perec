;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Persistent object base class

(defclass* persistent-object ()
  ((oid
    nil
    :type oid
    :persistent #f
    :documentation "Life time unique identifier of the object which can be remembered and may be used the load the object later.")
   (persistent
    :type boolean
    :persistent #f
    :documentation "True means the object is known to be persistent, false means the object is known to be transient, unbound means the state is not yet determined. Actually, in the latter case slot-value-using-class will automatically determine whether the object is in the database or not. Therefore reading the persistent slot will always return either true or false.")
   (transaction
    nil
    :accessor #f
    :type t
    :persistent #f
    :documentation "A weak reference to the transaction to this object is currently attached to.")
   (created
    #f
    :type boolean
    :persistent #f
    :documentation "True means the object was created in the current transaction.")
   (modified
    #f
    :type boolean
    :persistent #f
    :documentation "True means the object was modified in the current transaction.")
   (deleted
    #f
    :type boolean
    :persistent #f
    :documentation "True means the object was deleted in the current transaction.")
   (cached-slots
    nil
    :type (list persistent-effective-slot-definition)
    :persistent #f
    :documentation "A list of slots for which the slot values are currently cached in the object in the lisp VM. This list must be updated when database update happens outside of slot access (batch update, trigger, etc."))
  (:default-initargs :persistent #t)
  (:metaclass persistent-class))

(defprint-object (self persistent-object)
  "Prints the oid of the object and whether the object is known to be persistent or transient."
  (princ ":persistent ")
  (princ (cond ((not (slot-boundp self 'persistent))
                "#? ")
               ((persistent-p self)
                "#t ")
               (t "#f ")))
  (if (and (slot-boundp self 'oid)
           (oid-of self))
      (princ (id-of self))
      (princ "nil")))

(defun ensure-oid (object)
  "Makes sure that the object has a valid oid."
  (unless (oid-of object)
    (setf (oid-of object) (make-new-oid (class-name-of object)))))

(defun id-of (object)
  "Shortcut for the unique identifier number of the object."
  (oid-id (oid-of object)))

(defun (setf class-name-of) (new-value object)
  "Shortcut for the setter of the class name of the object."
  (setf (oid-class-name (oid-of object)) new-value))

(defun id-value (object)
  "Returns the RDBMS representation."
  (id-of object))

(defun class-name-value (object)
  "Returns the RDBMS representation."
  (canonical-symbol-name (class-name-of object)))

(defun oid-values (object)
  "Returns a list representation of the object oid in the order of the corresponding RDBMS columns."
  (list (id-value object) (class-name-value object)))
