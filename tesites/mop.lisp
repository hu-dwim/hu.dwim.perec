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
                                         &key instance persistent association time-dependent temporal integrated-slot-name &allow-other-keys)
  (cond (instance
         (class-of instance))
        ((and association
              (or time-dependent temporal))
         (find-class 'persistent-association-end-direct-slot-definition-t))
        ((and persistent
              (or time-dependent temporal integrated-slot-name))
         (find-class 'persistent-direct-slot-definition-t))
        (t
         (call-next-method))))

(defmethod effective-slot-definition-class ((class persistent-class-t)
                                            &key instance association time-dependent temporal integrated-slot-name &allow-other-keys)
  (cond (instance
         (class-of instance))
        ((and association
              (or time-dependent temporal))
         (find-class 'persistent-association-end-effective-slot-definition-t))
        ((or time-dependent temporal integrated-slot-name)
         (find-class 'persistent-effective-slot-definition-t))
        (t
         (call-next-method))))

(defmethod compute-effective-slot-definition ((class persistent-class-t)
                                              slot-name
                                              direct-slot-definitions)
  (if (some (lambda (slot)
              (typep slot 'persistent-direct-slot-definition-t))
            direct-slot-definitions)
      (bind ((standard-initargs (compute-standard-effective-slot-definition-initargs class direct-slot-definitions))
             (slot-initargs (mappend (lambda (slot-option-name)
                                       (some #L(when ;; TODO: fragile guard check for association option
                                                   (find-slot (class-of !1) slot-option-name)
                                                 (slot-initarg-and-value !1 slot-option-name))
                                             direct-slot-definitions))
                                     '(temporal time-dependent integrated-slot-name association)))
             (initargs (append slot-initargs standard-initargs))
             (effective-slot-class (apply #'effective-slot-definition-class class :persistent #t initargs)))
        (apply #'make-instance effective-slot-class initargs))
      (call-next-method)))

(defmethod initialize-instance :after ((instance persistent-effective-slot-definition-t) &key &allow-other-keys)
  (assert (or (temporal-p instance)
              (time-dependent-p instance)
              (integrated-slot-name-of instance))))
