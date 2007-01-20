;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(defstruct oid
  "The oid holds sufficient information to access the same object at any time in the database unless it has been deleted. The oid of an object is constant for the whole lifetime of the object and cannot be changed. It is assigned when the object is first time stored in the database."
  (id
   nil
   :type (or null integer))
  (class-name
   nil
   :type (or null symbol)))

(defparameter +oid-sequence-name+ "_OID"
  "The name of the oid sequence in the relational database.")

(defparameter +id-column-name+ '_id
  "The RDBMS column name for the id slot.")

(defparameter +class-name-column-name+ '_class_name
  "The RDBMS column name for the class name slot.")

(defparameter +oid-column-names+ (list +id-column-name+ +class-name-column-name+)
  "All RDBMS column names for an oid.")

(defparameter *oid-sequence-exists* #f
  "Tells if the oid sequence exists in the relational database or not. This flag is initially false but will be set to true as soon as the sequence is created or seen in the database.")

(defun make-new-oid (class-name)
  "Creates a fresh new oid. It never returns the same oid or an oid already used."
  (or *oid-sequence-exists* (ensure-oid-sequence))
  (make-oid :id (sequence-next +oid-sequence-name+) :class-name class-name))

(defun ensure-oid-sequence ()
  (unless (sequence-exists-p +oid-sequence-name+)
    (create-sequence +oid-sequence-name+))
  (setf *oid-sequence-exists* #t))
