;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Persistent class and slot meta objects

(defcclass* persistent-class (standard-class)
  ((abstract
    (compute-as #f)
    :type boolean
    :documentation "An abstract persistent class cannot be instantiated but still can be used in associations and may have slots. Calling make-instance on an abstract persistent class will signal an error. On the other hand abstract classes might not have a primary table and thus handling the instances may require simpler or less SQL statements.")
   (persistent-direct-slots
    (compute-as (remove-if-not #L(typep !1 'persistent-direct-slot-definition) (class-direct-slots -self-)))
    :type (list persistent-direct-slot-definition)
    :documentation "The list of direct slots which are defined to be persistent in this class.")
   (persistent-effective-slots
    (compute-as (remove-if-not #L(typep !1 'persistent-effective-slot-definition) (class-slots -self-)))
    :type (list persistent-effective-slot-definition)
    :documentation "The list of effective slots which are turned out to be persistent in this class.")
   (primary-table
    (compute-as (compute-primary-table -self- -current-value-))
    :type table
    :documentation "The table which holds the oid and the data of the direct slots of this class. If the class is abstract and does not have any persistent direct slots then it will not have a primary table. A primary table if exists contains one and only one record per instance of its persistent class.")
   (primary-tables
    (compute-as (compute-primary-tables -self-))
    :type (list class-primary-table)
    :documentation "The smallest set of tables which hold all instances and only the instances of this class by having one and only one record per instance. This list may contain functional nodes such as union, append according to the required SQL operation. For classes which have a primary table this list contains only that table for other classes the list will contain some of the primary tables of the sub persistent classes.")
   (data-tables
    (compute-as (compute-data-tables -self-))
    :type (list table)
    :documentation "All the tables which hold data of an instance of this class. This list contains primary tables of the super persistent classes.")
   (prefetched-slots
    (compute-as (remove-if-not #'prefetched-p (persistent-effective-slots-of -self-)))
    :type (list persistent-effective-slot-definition)
    :documentation "List of effective slots which will be loaded and stored at once when loading an instance of this class.")
   (non-prefetched-slots
    (compute-as (remove-if #'prefetched-p (persistent-effective-slots-of -self-)))
    :type (list effective-slot)
    :documentation "List of effective slots which will be loaded and stored lazily and separately from other slots.")))

(defcclass* persistent-slot-definition (standard-slot-definition)
  ((table
    (compute-as (compute-table -self-))
    :documentation "This slots servers different purposes for direct and effective slot meta objects.")
   (columns
    (compute-as (compute-columns -self-))
    :documentation "This slots servers different purposes for direct and effective slot meta objects.")
   (prefetched
    :type boolean
    :computed-in compute-as
    :documentation "Prefetched slots are loaded from and stored into the database at once. A prefetched slot must be in a table which can be accessed using a where clause matching to the id of the object thus it must be in a data table. The default prefetched slot semantics can be overriden on a per direct slot basis.")
   (cached
    :type boolean
    :computed-in compute-as
    :documentation "All prefetched slots are cached slots but the opposite may not be true. When a cached slot is loaded it's value will be stored in the CLOS object for fast subsequent read operations. Also whenever a cached slot is set the value will be remembered. The default cached slot semantics can be overriden on a per direct slot basis.")))

(defclass* accessor ()
  ((where-clause
    :type function
    :documentation "This function provides the SQL where clause to access the slot in the RDBMS.")
   (transformer
    :type function
    :documentation "A function which transforms between a lisp object and the corresponding RDBMS data. The direction of the transformation depends on whether the accessor is a reader or a writer. See slot-value-from-rdbms-values and rdbms-values-from-slot-value.")))

(defcclass* persistent-direct-slot-definition
    (persistent-slot-definition standard-direct-slot-definition)
  ((table
    :type (or null table)
    :documentation "A table where this direct slot has its columns if any.")
   (columns
    :type (or null list)
    :documentation "A list of RDBMS columns created for this direct slot or nil.")))

(defcclass* persistent-effective-slot-definition
    (persistent-slot-definition standard-effective-slot-definition)
  ((table
    :type table
    :documentation "An RDBMS table which will be queried or updated to get and set the data of this slot.")
   (columns
    :type (list sql-column)
    :documentation "A list of RDBMS columns which will be queried or updated to get and set the data of this slot.")
   (reader
    (compute-as (compute-reader -self-))
    :type accessor
    :documentation "An accessor object which describes how to read this slot.")
   (writer
    (compute-as (compute-writer -self-))
    :type accessor
    :documentation "An accessor object which describes how to write this slot.")
   (data-table-slot
    (compute-as (compute-data-table-slot-p -self-))
    :type boolean
    :documentation "True means the slot can be loaded from one of the data tables of its class.")
   (prefetched
    (compute-as (data-table-slot-p -self-)))
   (cached
    (compute-as (or (prefetched-p -self-)
                    (class-or-unspecified-type-p (slot-definition-type -self-)))))))

;;;;;;;;;;;;;
;;; defpclass

(defmacro defpclass (name super-classes slots &rest options)
  `(defclass ,name ,super-classes , slots
    ,@(append (unless (find :metaclass options :key 'first)
                '((:metaclass persistent-class)))
              options)))

(defmacro defpclass* (name super-classes slots &rest options)
  `(defclass* ,name ,super-classes , slots
    ,@(append (unless (find :metaclass options :key 'first)
                '((:metaclass persistent-class)))
              options)))

;; :persistent is a slot definition option and may be set to #t or #f
(eval-always
  (pushnew :persistent *allowed-slot-definition-properties*))

;;;;;;;;;;
;;; Export

(defmethod export-to-rdbms ((class persistent-class))
;; TODO: (mapc #'exported-model-element-of (effective-super-generalization-elements-for class))
;; TODO: (mapc #'exported-model-element-of (associations-of class))
  (awhen (primary-table-of class)
    (export-to-rdbms it)))

;;;;;;;;;;;;
;;; Computed

(defun slot-definition-class (slot)
  (slot-value slot 'sb-pcl::%class))

(defgeneric compute-column-type (type)
  (:documentation "Returns the RDBMS type for the given type.")

  (:method (type)
           (error "Cannot map type ~A to RDBMS type" type)))

(defgeneric compute-reader (type)
  (:documentation "Maps types to reader transformers.")

  (:method (type)
           (error "Cannot map type ~A to a reader" type)))

(defgeneric compute-writer (type)
  (:documentation "Maps types to writer transformers.")

  (:method (type)
           (error "Cannot map type ~A to a writer" type))

  (:method ((type cons))
           (call-next-method (first type))))

(defgeneric compute-primary-table (class current-table)
  (:method ((class persistent-class) current-table)
           (bind ((primary-table (or current-table
                                     (make-instance 'class-primary-table
                                                    :name (rdbms-name-for (class-name class))))))
             (when (or (not (abstract-p class))
                       (mappend #'columns-of (persistent-direct-slots-of class)))
               primary-table))))

(defgeneric compute-primary-tables (class)
  (:method ((class persistent-class))
           #+nil(labels ((primary-entities-for (class)
                      (if (primary-table-of class)
                          (list class)
                          (iter (for sub-class in (direct-sub-generalization-elements-for class))
                                (appending (primary-entities-for sub-class))))))
             (bind ((primary-entities (primary-entities-for class))
                    (primary-class-sub-entities (mapcar #'effective-sub-generalization-elements-for primary-entities))
                    (primary-tables (mapcar #'primary-table-of primary-entities)))
               (when primary-class-sub-entities
                 (if (eq (length (reduce #'union primary-class-sub-entities))
                         (length (reduce #'append primary-class-sub-entities)))
                     (cons 'append primary-tables)
                     (cons 'union primary-tables)))))))

(defgeneric compute-data-tables (class)
  (:method ((class persistent-class))
           (delete-if #'null (mapcar #'primary-table-of (class-precedence-list class)))))

(defgeneric compute-table (slot)
  (:method ((slot persistent-direct-slot-definition))
           (primary-table-of (slot-definition-class slot)))

  (:method ((slot persistent-effective-slot-definition))
           #+nil(primary-table-of (slot-definition-class (most-generic-direct-slot-for slot)))))

(defgeneric compute-columns (slot)
  (:method ((slot persistent-direct-slot-definition))
           (when (slot-definition-type slot)
             (when-bind table (table-of slot)
               (make-columns-for-slot slot))))

  #+nil(:method ((slot persistent-effective-slot-definition))
                (columns-of (most-generic-direct-slot-for slot))))

(defun make-oid-columns ()
  "Creates a list of RDBMS columns that will be used to store the oid data of the objects in this table."
  (bind ((*package* (find-package :dwim-meta-model)))
    (list
     (make-instance 'sql-column
                    :name +id-column-name+
                    :type +oid-id-sql-type+
                    :constraints (list (make-instance 'sql-not-null-constraint)
                                       (make-instance 'sql-primary-key-constraint)))
     (make-instance 'sql-column
                    :name +class-name-column-name+
                    :type +oid-class-name-sql-type+))))

(defun make-columns-for-reference-slot (slot)
  (list
   ;; TODO: add an RDBMS index and we may also need to add an RDBMS unique constraint? 
   (make-instance 'sql-column
                  :name (rdbms-name-for (slot-definition-name slot)) 
                  :type +oid-id-sql-type+)
   (make-instance 'sql-column
                  :name (rdbms-name-for (slot-definition-name slot))
                  :type +oid-class-name-sql-type+)))

(defgeneric make-columns-for-slot (slot)
  (:method ((slot persistent-direct-slot-definition))
           (awhen (slot-definition-type slot)
             (list
              (make-instance 'sql-column
                             :name (rdbms-name-for (slot-definition-name slot))
                             :type (compute-column-type it)))))

  #+nil(:method ((slot direct-slot))
                (when (declared-slot-type-of slot)
                  (bind ((slot-type (slot-type-of slot)))
                    (cond ((entity-or-unspecified-type-p slot-type)
                           (make-columns-for-reference-slot slot))
                          (t
                           (error "Unknown slot type ~A" slot-type)))))))

(defgeneric compute-data-table-slot-p (slot)
  (:method ((slot persistent-effective-slot-definition))
           #t))
