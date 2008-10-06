;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(defmethod validate-superclass ((class persistent-class) (superclass persistent-class-d))
  t)

(defmethod validate-superclass ((class persistent-class-d) (superclass persistent-class))
  t)

(defmethod direct-slot-definition-class ((class persistent-class-d)
                                         &key instance persistent association dimension &allow-other-keys)
  (cond (instance
         (class-of instance))
        ((and persistent
              (not association)
              dimensions)
         (find-class 'persistent-direct-slot-definition-d))
        (t
         (call-next-method))))

(defmethod direct-slot-definition-class ((class persistent-association-d) &key dimensions &allow-other-keys)
  (if dimensions
      (find-class 'persistent-association-end-direct-slot-definition-d)
      (call-next-method)))

(defmethod effective-slot-definition-class ((class persistent-class-d)
                                            &key instance persistent association dimensions &allow-other-keys)
  (cond (instance
         (class-of instance))
        ((and persistent
              (not association)
              dimensions)
         (find-class 'persistent-effective-slot-definition-d))
        (t
         (call-next-method))))

(defmethod effective-slot-definition-class ((class persistent-association-d) &key dimensions &allow-other-keys)
  (if dimensions
      (find-class 'persistent-association-end-effective-slot-definition-d)
      (call-next-method)))

(defmethod compute-persistent-effective-slot-definition-option ((class persistent-class-d)
                                                                (direct-slot persistent-direct-slot-definition-d)
                                                                slot-option-name
                                                                direct-slot-definitions)
  (if (eq slot-option-name 'dimensions)
      (merge-dimensions [second (slot-initarg-and-value !1 slot-option-name)] direct-slot-definitions)
      (call-next-method)))

(defmethod compute-persistent-effective-slot-definition-option ((class persistent-class)
                                                                (direct-slot persistent-association-end-direct-slot-definition-d)
                                                                slot-option-name
                                                                direct-slot-definitions)
  (if (member slot-option-name '(temporal time-dependent))
      (merge-dimensions [second (slot-initarg-and-value !1 slot-option-name)] direct-slot-definitions)
      (call-next-method)))

(defmethod initialize-instance :after ((instance persistent-effective-slot-definition-d) &key &allow-other-keys)
  (assert (dimensions-of instance)))

(defmethod persistent-class-default-superclass ((class persistent-class-d))
  (find-class 'd-object nil))

(def function merge-dimensions (dimensions-list)
  (bind ((dimensions (find-if identity dimensions-list)))
    (assert (every [or (null !1) (equal dimensions !1)] dimensions-list)
            () "Dimensions cannot be overridden. Received: ~S" dimensions-list)
    dimensions))
