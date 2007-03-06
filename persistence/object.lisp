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
  (:default-initargs :persistent *make-persistent-instances*)
  (:abstract #t)
  (:documentation "Base class for all persistent classes. If this class is not inherited by a persistent class then it is automatically added to the direct superclasses. There is only one persistent object instance in a transaction with a give oid therefore eq will return true iff the oids are equal."))

(defmacro with-making-persistent-instances (&body forms)
  `(let ((*make-persistent-instances* #t))
    ,@forms))

(defmacro with-making-transient-instances (&body forms)
  `(let ((*make-persistent-instances* #f))
    ,@forms))

;;;;;;;;;;;;;;;
;;; MOP methods

(defmethod shared-initialize :around ((object persistent-object) slot-names &rest args
                                      &key (initialize-persistent-slots #t) persistent &allow-other-keys)
  "Make sure that this slot is set before any other slots, especially persistent ones."
  ;; make-persistent will be called from initialize-instance
  ;; this allows svuc to cache initargs and ignore the database
  (remf-keywords args :persistent :cached-slots :initialize-persistent-slots)
  (let ((processed-slot-names
         (if initialize-persistent-slots
             slot-names
             (if (eq slot-names t)
                 ;; TODO: cache the list and make this more efficient
                 (iter (for slot in (class-slots (class-of object)))
                       (unless (typep slot 'persistent-slot-definition)
                         (collect (slot-definition-name slot))))
                 (remove-if #L(typep (find-slot (class-of object) !1) 'persistent-slot-definition) slot-names)))))
    (prog1 (apply #'call-next-method object processed-slot-names :persistent #f args)
      (when (eq persistent 'unknown)
        (slot-makunbound object 'persistent)))))

(defmethod initialize-instance :before ((object persistent-object) &key persistent &allow-other-keys)
  (when persistent
    (ensure-exported (class-of object))))

(defmethod initialize-instance :after ((object persistent-object) &key persistent &allow-other-keys)
  (when (eq persistent #t)
    (make-persistent object)
    (setf (created-p object) #t)
    (setf (cached-slots-of object)
          (collect-if #'cache-p (persistent-effective-slots-of (class-of object))))))

(defmethod make-instance :before ((class persistent-class) &key &allow-other-keys)
  (when (abstract-p class)
    (error "Cannot make instances of abstract class ~A" class)))

;;;;;;;;;;;
;;; Utility

(defvar +persistent-object-class+ (find-class 'persistent-object))

(defun persistent-object-p (object)
  (typep object 'persistent-object))

(defun p-eq (object-1 object-2)
  "Tests if two object references the same persistent object. Normally there at most one persistent object for each oid in a transaction so eq may be safely used. On the other hand huge transactions may require to throw away objects form the object cache which results in several instances for the same oid within the same transaction."
  (or (eq object-1 object-2)
      (= (id-of object-1)
         (id-of object-2))))

(defun print-persistent-object (object)
  (declare (type persistent-object object))
  (princ ":persistent ")
  (princ (cond ((not (slot-boundp object 'persistent))
                "#? ")
               ((persistent-p object)
                "#t ")
               (t "#f ")))
  (if (and (slot-boundp object 'oid)
           (oid-of object))
      (princ (id-of object))
      (princ "nil")))

(defprint-object (self persistent-object)
  "Prints the oid of the object and whether the object is known to be persistent or transient."
  (print-persistent-object self))

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
