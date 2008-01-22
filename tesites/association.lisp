;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(defcclass* persistent-association-t (persistent-association)
  ())

(defcclass* persistent-association-end-slot-definition-t (persistent-slot-definition-t)
  ;; TODO: kill association?!
  ((association)))

(defcclass* persistent-association-end-direct-slot-definition-t
    (persistent-association-end-slot-definition-t persistent-direct-slot-definition-t)
  ()
  (:metaclass identity-preserving-class))

(defcclass* persistent-association-end-effective-slot-definition-t
    (persistent-association-end-slot-definition-t persistent-effective-slot-definition-t)
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
