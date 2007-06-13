;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(defstruct oid
  "The oid holds sufficient information to access the same persistent instance at any time in the database unless it has been deleted. The instance id of an instance is constant and unique for the whole lifetime of the instance and cannot be changed. It is assigned when the persistent instance is first time stored in the database."
  (id ;; stored in the id columns; either the instance id or the merged value of the instance and the class id
   nil
   :type (or null integer))
  (instance-id ;; a life time constant and unique identifier
   nil
   :type (or null integer))
  (class-id ;; a constant and unique class identifier kept between sessions
   nil
   :type (or null fixnum))
  (class-name ;; the name of the class
   nil
   :type (or null symbol)))

(defparameter +oid-sequence-name+ "_OID"
  "The name of the oid sequence in the relational database used to generate life time unique identifiers for all persistent instances.")

(defconstant +oid-id-column-name+ '_id
  "The RDBMS table column name for the oid's id slot.")

(defconstant +oid-class-id-column-name+ '_class_id
  "The RDBMS table column name for the oid's class id slot.")

(defconstant +oid-class-name-column-name+ '_class_name
  "The RDBMS table column name for the oid's class name slot.")

(defparameter +oid-column-names+ (list +id-column-name+ +class-name-column-name+)
  "All RDBMS table column names for an oid. The order of columns does matter.")

(defparameter *oid-sequence-exists* #f
  "Tells if the oid sequence exists in the relational database or not. This flag is initially false but will be set to true as soon as the sequence is created or first time seen in the database.")

(defun make-class-oid (class-name)
  "Creates a fresh and unique oid which was never used before in the relational database."
  (or *oid-instance-id-sequence-exists* (ensure-instance-id-sequence))
  (bind ((instance-id (next-instance-id))
         (class-id (class-name->class-id class-name))
         (id (oid-mode-ecase
               (:class-name instance-id)
               (:class-id instance-id)
               (:merge (logior (ash instance-id +oid-class-id-bit-size+) class-id)))))
    (make-oid :id id
              :instance-id instance-id
              :class-id class-id
              :class-name class-name)))

(defun ensure-instance-id-sequence ()
  "Makes sure the instance id sequence exists in the database."
  (unless (sequence-exists-p +oid-instance-id-sequence-name+)
    (create-sequence +oid-instance-id-sequence-name+))
  (setf *oid-instance-id-sequence-exists* #t))

(defun next-instance-id ()
  (aprog1 (sequence-next +oid-instance-id-sequence-name+)
    (assert (<= (integer-length it) +oid-instance-id-bit-size+))))

(defun next-class-id ()
  (aprog1 (sequence-next +oid-class-id-sequence-name+)
    (assert (< it +oid-maximum-class-id+))))

(defun ensure-class-id-sequence ()
  "Makes sure the class id sequence exists in the database."
  (unless (sequence-exists-p +oid-class-id-sequence-name+)
    (create-sequence +oid-class-id-sequence-name+))
  (setf *oid-class-id-sequence-exists* #t))

(defun oid->rdbms-values (oid)
  "Returns a sufficient list representation for relational database operations of the instance's oid in the order of the corresponding RDBMS columns."
  (bind ((id (oid-id oid)))
    (oid-mode-ecase
      (:class-name (list id (oid-class-name oid)))
      (:class-id (list id (oid-class-id oid)))
      (:merge (list id)))))

(defun rdbms-values->oid (rdbms-values)
  (oid-mode-ecase
    (:class-name
     (bind ((instance-id (first rdbms-values)))
       (make-oid :id instance-id
                 :instance-id (first rdbms-values)
                 :class-name (symbol-from-canonical-name (second rdbms-values)))))
    (:class-id
     (bind ((instance-id (first rdbms-values))
            (class-id (second rdbms-values)))
       (make-oid :id instance-id
                 :instance-id instance-id 
                 :class-id class-id
                 :class-name (class-id->class-name class-id))))
    (:merge
     (bind ((id (first rdbms-values))
            (instance-id (ldb (byte +oid-instance-id-bit-size+ +oid-class-id-bit-size+) id))
            (class-id (ldb (byte +oid-class-id-bit-size+ 0) id)))
       (make-oid :id id
                 :instance-id instance-id
                 :class-id class-id
                 :class-name (class-id->class-name class-id))))))
