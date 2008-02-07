;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(defmethod validate-superclass ((class persistent-class)
                                (superclass persistent-class-t))
  t)

(defmethod validate-superclass ((class persistent-class-t)
                                (superclass persistent-class))
  t)

(defmethod direct-slot-definition-class ((class persistent-class-t)
                                         &key instance persistent association time-dependent temporal &allow-other-keys)
  (cond (instance
         (class-of instance))
        ((and association
              (or time-dependent temporal))
         (find-class 'persistent-association-end-direct-slot-definition-t))
        ((and persistent
              (or time-dependent temporal))
         (find-class 'persistent-direct-slot-definition-t))
        (t
         (call-next-method))))

(defmethod effective-slot-definition-class ((class persistent-class-t)
                                            &key instance persistent association time-dependent temporal &allow-other-keys)
  (cond (instance
         (class-of instance))
        ((and association
              (or time-dependent temporal))
         (find-class 'persistent-association-end-effective-slot-definition-t))
        ((and persistent
              (or time-dependent temporal))
         (find-class 'persistent-effective-slot-definition-t))
        (t
         (call-next-method))))

(defmethod compute-persistent-effective-slot-definition-option ((class persistent-class-t)
                                                                (direct-slot persistent-direct-slot-definition-t)
                                                                slot-option-name
                                                                direct-slot-definitions)
  (if (member slot-option-name '(temporal time-dependent))
      (some #L(slot-initarg-and-value !1 slot-option-name) direct-slot-definitions)
      (call-next-method)))

(defmethod initialize-instance :after ((instance persistent-effective-slot-definition-t) &key &allow-other-keys)
  (assert (or (temporal-p instance)
              (time-dependent-p instance))))

(defmethod persistent-class-default-superclass ((class persistent-class-t))
  (find-class 't-object nil))
