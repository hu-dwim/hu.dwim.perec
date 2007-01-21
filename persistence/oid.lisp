;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(defstruct oid
  "The oid holds sufficient information to access the same persistent object at any time in the database unless it has been deleted. The oid of an object is constant for the whole lifetime of the object and cannot be changed. It is assigned when the persistent object is first time stored in the database."
  (id
   nil
   :type (or null integer))
  (class-name
   nil
   :type (or null symbol)))

(defparameter +oid-sequence-name+ "_OID"
  "The name of the oid sequence in the relational database used to generate life time unique identifiers for all persistent objects.")

(defparameter +id-column-name+ '_id
  "The RDBMS table column name for the oid's id slot.")

(defparameter +class-name-column-name+ '_class_name
  "The RDBMS table column name for the oid's class name slot.")

(defparameter +oid-column-names+ (list +id-column-name+ +class-name-column-name+)
  "All RDBMS table column names for an oid. The order of columns does matter.")

(defparameter *oid-sequence-exists* #f
  "Tells if the oid sequence exists in the relational database or not. This flag is initially false but will be set to true as soon as the sequence is created or first time seen in the database.")

(defun make-new-oid (class-name)
  "Creates a fresh new oid which was never used before. It never returns the same oid or an oid that is already used."
  (or *oid-sequence-exists* (ensure-oid-sequence))
  (make-oid :id (sequence-next +oid-sequence-name+) :class-name class-name))

(defun ensure-oid-sequence ()
  "Makes sure the oid sequence exists in the database."
  (unless (sequence-exists-p +oid-sequence-name+)
    (create-sequence +oid-sequence-name+))
  (setf *oid-sequence-exists* #t))
