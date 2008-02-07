;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Persistent association t and slot meta objects

(defcclass* persistent-association-t (persistent-association)
  ())

(defcclass* persistent-association-end-slot-definition-t (persistent-slot-definition-t persistent-association-end-slot-definition)
  ())

(defcclass* persistent-association-end-direct-slot-definition-t
    (persistent-association-end-slot-definition-t persistent-direct-slot-definition-t persistent-association-end-direct-slot-definition)
  ()
  (:metaclass identity-preserving-class))

(defcclass* persistent-association-end-effective-slot-definition-t
    (persistent-association-end-slot-definition-t persistent-effective-slot-definition-t persistent-association-end-effective-slot-definition)
  ((h-class
    (compute-as (h-class-of (slot-definition-class -self-))) ;; TODO: hack
    :type persistent-class)
   (table
    (compute-as (primary-table-of (h-class-of -self-)))
    :type table)
   (t-value-column
    (compute-as (first (columns-of (find-slot (h-class-of -self-) 't-value))))
    :type column)
   (validity-start-column
    (compute-as (first (columns-of (find-slot (h-class-of -self-) 'validity-start))))
    :type column)
   (validity-end-column
    (compute-as (first (columns-of (find-slot (h-class-of -self-) 'validity-end))))
    :type column)
   (action-column
    (compute-as (first (columns-of (find-slot (h-class-of -self-) 'action))))
    :type column)
   (parent-oid-columns
    (compute-as (columns-of (find-slot (h-class-of -self-) (class-name (slot-definition-class -self-)))))
    :type list)
   (child-oid-columns
    (compute-as (columns-of (child-slot-of -self-)))
    :type list)
   (child-slot
    (compute-as (find-slot (h-class-of -self-) (slot-definition-name -self-)))
    :type list)
   (action-slot
    (compute-as (find-slot (h-class-of -self-) 'action))
    :type persistent-effective-slot-definition)))

;;;;;;;;;;;;;;;;;;
;;; defassociation

(defmethod expand-defassociation-form :around ((metaclass null) association-ends options)
  (bind ((specified-metaclass (second (find :metaclass options :key #'first)))
         (processed-options
          (if (and (not specified-metaclass)
                   (or (find :temporal options :key #'first)
                       (find :time-dependent options :key #'first)))
              (append options '((:metaclass persistent-association-t)))
              options)))
    (call-next-method metaclass association-ends processed-options)))

(defmethod expand-defassociation-form ((metaclass persistent-association-t) association-ends options)
  (with-decoded-association-ends association-ends
    (bind ((superclasses
            (append (when (find :temporal options :key #'first)
                      (list 'temporal-object))
                    (when (find :time-dependent options :key #'first)
                      (list 'time-dependent-object)))))
      `(progn
         ,(call-next-method)
         (defpclass* ,association-name ,superclasses
           ())
         (defassociation*
           ((:class ,association-name :slot ,(concatenate-symbol "t-" secondary-slot *package*) :type ,primary-class)
            (:class ,primary-class :slot ,(concatenate-symbol "h-" primary-slot *package*) :type (set ,association-name))))
         (defassociation*
           ((:class ,association-name :slot ,(concatenate-symbol "t-" primary-slot *package*) :type ,secondary-class)
            (:class ,secondary-class :slot ,(concatenate-symbol "h-" secondary-slot *package*) :type (set ,association-name))))))))
