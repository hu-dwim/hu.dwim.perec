;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;;;;;;;;;
;;; MOP methods 

(def method validate-superclass ((class persistent-class) (superclass persistent-class-d))
  t)

(def method validate-superclass ((class persistent-class-d) (superclass persistent-class))
  t)

(def method validate-superclass ((class persistent-class) (superclass persistent-class-h))
  t)

(def method validate-superclass ((class persistent-class-h) (superclass persistent-class))
  t)

(def method direct-slot-definition-class ((class persistent-class-d)
                                         &key instance persistent association dimensions &allow-other-keys)
  (cond (instance
         (class-of instance))
        ((and persistent
              (not association)
              dimensions)
         (find-class 'persistent-direct-slot-definition-d))
        (t
         (call-next-method))))

(def method direct-slot-definition-class ((class persistent-association-d) &key dimensions &allow-other-keys)
  (if dimensions
      (find-class 'persistent-association-end-direct-slot-definition-d)
      (call-next-method)))

(def method effective-slot-definition-class ((class persistent-class-d)
                                            &key instance persistent association dimensions &allow-other-keys)
  (cond (instance
         (class-of instance))
        ((and persistent
              (not association)
              dimensions)
         (find-class 'persistent-effective-slot-definition-d))
        (t
         (call-next-method))))

(def method effective-slot-definition-class ((class persistent-association-d) &key dimensions &allow-other-keys)
  (if dimensions
      (find-class 'persistent-association-end-effective-slot-definition-d)
      (call-next-method)))

(def method compute-persistent-effective-slot-definition-option ((class persistent-class-d) (direct-slot persistent-direct-slot-definition-d)
                                                                 slot-option-name
                                                                 direct-slot-definitions)
  (if (eq slot-option-name 'dimensions)
      (list :dimensions (merge-dimensions (mapcar #'dimensions-of direct-slot-definitions)))
      (call-next-method)))

(def method initialize-instance :after ((instance persistent-effective-slot-definition-d) &key &allow-other-keys)
  (assert (dimensions-of instance)))

(def method persistent-class-default-superclasses ((class persistent-class-d) class-name)
  (unless (eq class-name 'd-object)
    (list (find-class 'd-object))))

(def function merge-dimensions (dimensions-list)
  (bind ((dimensions (first dimensions-list)))
    (assert (every [or (null !1) (equal dimensions !1)] dimensions-list)
            nil "Dimensions cannot be overridden. Received: ~S" dimensions-list)
    dimensions))

(def method persistent-class-default-superclasses ((class persistent-class-h) h-class-name)
  (bind ((d-class-name (h-class-name->d-class-name h-class-name)))
    (append (mapcar (lambda (d-class-name)
                      (find-class (d-class-name->h-class-name d-class-name)))
                    (remove-if-not (of-type 'persistent-class-d) 
                                   (class-direct-superclasses (find-class d-class-name))))
            (call-next-method))))
