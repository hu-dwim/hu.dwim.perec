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
    :documentation "The smallest set of tables which hold all instances and only the instances of this class by having one and only one record per instance. This list may contain functional nodes such as union, append according to the required SQL operation. For classes which have a primary table this list contains only that table while for other classes the list will contain some of the primary tables of the sub persistent classes.")
   (data-tables
    (compute-as (compute-data-tables -self-))
    :type (list table)
    :documentation "All the tables which hold direct data of an instance of this class. This list contains the primary tables of the super persistent classes.")
   (prefetched-slots
    (compute-as (remove-if-not #'prefetched-p (persistent-effective-slots-of -self-)))
    :type (list persistent-effective-slot-definition)
    :documentation "The list of effective slots which will be loaded from and stored to the database at once when loading an instance of this class. Moreover when a persistent object is revived its prefetched slots will be loaded.")
   (non-prefetched-slots
    (compute-as (remove-if #'prefetched-p (persistent-effective-slots-of -self-)))
    :type (list effective-slot)
    :documentation "The list of effective slots which will be loaded and stored lazily and separately from other slots.")
   (ensure-exported
    (compute-as (export-to-rdbms -self-))
    :reader ensure-exported
    :type persistent-class
    :documentation "The persistent class must be exported before use. This will automatically happen not later than making, reviving or querying the first instance of it.")
   (depends-on
    (compute-as nil)
    :type (list persistent-class)
    :documentation "The list of persistent classes which must be look at by this class when computing RDBMS meta data. This used to generate columns into other classes' primary tables.")
   (depends-on-me
    (compute-as nil)
    :type (list persistent-class)
    :documentation "The list of persistent classes which must look at this class when computing RDBMS meta data."))
  (:documenation "Persistent class is a class meta object for classes. Standard defclass forms may be used to define persistent classes. A persistent class will have persistent slots unless marked with :persistent #f. A persistent slot should have type specification to be efficient both in storage and speed. The special type unbound must be used to mark slots which might be unbound."))

(defcclass* persistent-slot-definition (standard-slot-definition)
  ((table
    (compute-as (compute-table -self-))
    :documentation "This slot servers different purposes for direct and effective slot meta objects.")
   (columns
    (compute-as (compute-columns -self-))
    :documentation "This slot servers different purposes for direct and effective slot meta objects.")
   (prefetched
    :type boolean
    :computed-in compute-as
    :documentation "Prefetched slots are loaded from and stored into the database at once. A prefetched slot must be in a table which can be accessed using a where clause matching to the id of the object thus it must be in a data table. The default prefetched slot semantics can be overriden on a per direct slot basis.")
   (cached
    :type boolean
    :computed-in compute-as
    :documentation "All prefetched slots are cached slots but the opposite may not be true. When a cached slot is loaded it's value will be stored in the CLOS object for fast subsequent read operations. Also whenever a cached slot is set the value will be remembered. The default cached slot semantics can be overriden on a per direct slot basis."))
  (:documenation "Base class for both persistent direct and effective slot definitions."))

(defclass* accessor ()
  ((where-clause
    :type function
    :documentation "A one parameter function called with the object which provides the SQL where clause when accessing the slot in the RDBMS.")
   (transformer
    :type function
    :documentation "A two parameter function which transforms between a lisp object and the corresponding RDBMS data. The direction of the transformation depends on whether the accessor is a reader or a writer. See slot-value-from-rdbms-values and rdbms-values-from-slot-value how they are used.")))

(defcclass* persistent-direct-slot-definition
    (persistent-slot-definition standard-direct-slot-definition)
  ((table
    :type (or null table)
    :documentation "A table where this direct slot has its columns if any. If the slot does not have a type specification then this is nil.")
   (columns
    :type (or null list)
    :documentation "A list of RDBMS columns created for this direct slot or nil. If the slot does not have a type specification then this is nil."))
  (:documenation "Class for persistent direct slot definitions."))

(defcclass* persistent-effective-slot-definition
    (persistent-slot-definition standard-effective-slot-definition)
  ((direct-slots
    :type (list persistent-direct-slot-definition)
    :documentation "The list of direct slots definitions used to compute this effective slot during the class finalization procedure.")
   (table
    :type table
    :documentation "The RDBMS table which will be queried or updated to get and set the data of this slot.")
   (columns
    :type (list sql-column)
    :documentation "The list of RDBMS columns which will be queried or updated to get and set the data of this slot.")
   (reader
    (compute-as (compute-reader -self-))
    :type accessor
    :documentation "The accessor object which describes how to read this slot.")
   (writer
    (compute-as (compute-writer -self-))
    :type accessor
    :documentation "The accessor object which describes how to write this slot.")
   (data-table-slot
    (compute-as (compute-data-table-slot-p -self-))
    :type boolean
    :documentation "True means the slot can be loaded from one of the data tables of its class.")
   (prefetched
    (compute-as (data-table-slot-p -self-))
    :documentation "The prefetched option is inherited among direct slots according to the class precedence list. If no direct slot has prefetched specification then the default behaviour is to prefetch data tabe slot.")
   (cached
    (compute-as (or (prefetched-p -self-)
                    (persistent-class-type-p (remove-null-and-unbound-if-or-type (slot-definition-type -self-)))))
    :documentation "The cached option is inherited among direct slots according to the class precedence list. If no direct slot has cached specification then the default behaviour is to cache prefetched slots and single object references."))
  (:documenation "Class for persistent effective slot definitions."))

;;;;;;;;;;
;;; Export

(defmethod export-to-rdbms ((class persistent-class))
  (mapc #'ensure-exported (remove-if-not #L(typep !1 'persistent-class) (cdr (class-precedence-list class))))
  ;; TODO: (mapc #'exported-model-element-of (associations-of class))
  (awhen (primary-table-of class)
    (export-to-rdbms it)))

;;;;;;;;;;;;
;;; Computed

(defun slot-definition-class (slot)
  "Returns the class to which the given slot belongs."
  #+sbcl(slot-value slot 'sb-pcl::%class)
  #-sbcl(not-yet-implemented))

(defun remove-null-and-unbound-if-or-type (type)
  "If the type is an or type specification then it removes null and unbound from it. Removes the or type combinator from the result if possible."
  (if (and (listp type)
           (eq 'or (first type)))
      (let ((simplified-type (remove 'null (remove 'unbound type))))
        (if (<= (length simplified-type) 2)
            (second simplified-type)
            simplified-type))
      type))

(defun primitive-type-p (type)
  "Accept types such as: integer, string, boolean, (or unbound integer), (or null string), (or unbound null boolean), etc."
  (cond ((listp type)
         (member (first type) '(serialized string symbol symbol* member form)))
        ((symbolp type)
         (not (subtypep type 'persistent-object)))
        (t #f)))

(defun persistent-class-type-p (type)
  "Returns true for persistent class types."
  (subtypep type 'persistent-object))

(defun set-type-p (type)
  "Returns true for persistent set types."
  (and (listp type)
       (eq 'set (first type))))

(defgeneric compute-column-type (type &optional type-specification)
  (:documentation "Returns the RDBMS type for the given type.")

  (:method (type &optional type-specification)
           (declare (ignore type-specification))
           (error "Cannot map type ~A to RDBMS type" type))

  (:method ((type list) &optional type-specification)
           (declare (ignore type-specification))
           (compute-column-type (first type) type)))

(defgeneric compute-reader-transformer (type &optional type-specification)
  (:documentation "Maps a type to a two parameters lambda which will be called with the object and the received RDBMS values.")

  (:method (type &optional type-specification)
           (declare (ignore type-specification))
           (error "Cannot map type ~A to a reader" type))

  (:method ((type list) &optional type-specification)
           (declare (ignore type-specification))
           (compute-reader-transformer (first type) type))

  (:method ((type symbol) &optional type-specification)
           (declare (ignore type-specification))
           (if (subtypep type 'persistent-object)
               'object-reader
               (call-next-method)))

  (:method ((type (eql 'or)) &optional type-specification)
           (let ((type (remove-null-and-unbound-if-or-type type-specification)))
             (compute-writer-transformer type))))

;; TODO: maybe eliminate object parameter too
(defgeneric compute-writer-transformer (type &optional type-specification)
  (:documentation "Maps a type to a two parameters lambda which will be called with the object and the slot value.")

  (:method (type &optional type-specification)
           (declare (ignore type-specification))
           (error "Cannot map type ~A to a writer" type))

  (:method ((type list) &optional type-specification)
           (declare (ignore type-specification))
           (compute-writer-transformer (first type) type))

  (:method ((type symbol) &optional type-specification)
           (declare (ignore type-specification))
           (if (persistent-class-type-p type)
               'object-writer
               (call-next-method)))

  (:method ((type (eql 'or)) &optional type-specification)
           (let ((type (remove-null-and-unbound-if-or-type type-specification)))
             (compute-writer-transformer type))))

(defgeneric compute-reader-where-clause (slot type &optional type-specification)
  (:documentation "Maps a type to a one parameter lambda which will be called with the object")

  (:method (slot type &optional type-specification)
           (declare (ignore slot type-specification))
           (error "Cannot map type ~A to a reader where clause" type))

  (:method (slot (type list) &optional type-specification)
           (declare (ignore type-specification))
           (compute-reader-where-clause slot (first type) type))

  (:method ((slot persistent-effective-slot-definition) (type symbol) &optional type-specification)
           (declare (ignore slot type type-specification))
           'oid-matcher-reader-where-clause)

  (:method ((slot persistent-effective-slot-definition) (type (eql 'set)) &optional type-specification)
           (declare (ignore type-specification))
           (make-association-end-matcher-reader-where-clause
            (rdbms::name-of (first (columns-of slot))))))

(defgeneric compute-writer-where-clause (slot type &optional type-specification)
  (:method (slot type &optional type-specification)
           (declare (ignore slot type-specification))
           (error "Cannot map type ~A to a writer where clause" type))

  (:method (slot (type list) &optional type-specification)
           (declare (ignore type-specification))
           (compute-writer-where-clause slot (first type) type))

  (:method ((slot persistent-effective-slot-definition) (type symbol) &optional type-specification)
           (declare (ignore slot type type-specification))
           'oid-matcher-writer-where-clause)

  (:method ((slot persistent-effective-slot-definition) (type (eql 'set)) &optional type-specification)
           (declare (ignore slot type-specification))
           'value-matcher-writer-where-clause))

(defgeneric compute-reader (slot)
  (:method ((slot persistent-effective-slot-definition))
           (let ((type (slot-definition-type slot)))
             (make-instance 'accessor
                            :where-clause (compute-reader-where-clause slot type)
                            :transformer (compute-reader-transformer type)))))

(defgeneric compute-writer (slot)
  (:method ((slot persistent-effective-slot-definition))
           (let ((type (slot-definition-type slot)))
             (make-instance 'accessor
                            :where-clause (compute-writer-where-clause slot type)
                            :transformer (compute-writer-transformer type)))))

(defgeneric compute-primary-table (class current-table)
  (:method ((class persistent-class) current-table)
           (flet ((primary-table-columns-for-class (class)
                    (mappend #L(mappend #L(when (eq (primary-table-of class) (table-of !1))
                                            (columns-of !1))
                                        (persistent-direct-slots-of !1))
                             (list* class (depends-on-of class)))))
             (when (or (not (abstract-p class))
                       (primary-table-columns-for-class class))
               (or current-table
                   (make-instance 'class-primary-table
                                  :name (rdbms-name-for (class-name class))
                                  :columns (compute-as
                                             (append
                                              (make-oid-columns)
                                              (primary-table-columns-for-class class)))))))))

(defgeneric compute-primary-tables (class)
  (:method ((class persistent-class))
           (labels ((primary-classes-for (class)
                      (if (primary-table-of class)
                          (list class)
                          (iter (for subclass in (class-direct-subclasses class))
                                (appending (primary-classes-for subclass)))))
                    (class-effective-subclasses (class)
                      (aif (class-direct-subclasses class)
                           (mappend #'class-effective-subclasses it)
                           (list class))))
             (bind ((primary-classes (primary-classes-for class))
                    (primary-class-sub-classes (mapcar #'class-effective-subclasses primary-classes))
                    (primary-tables (mapcar #'primary-table-of primary-classes)))
               (when primary-class-sub-classes
                 (if (eq (length (reduce #'union primary-class-sub-classes))
                         (length (reduce #'append primary-class-sub-classes)))
                     (cons 'append primary-tables)
                     (cons 'union primary-tables)))))))

(defgeneric compute-data-tables (class)
  (:method ((class persistent-class))
           (delete-if #'null
                      (mapcar #'primary-table-of
                              (remove-if-not #L(typep !1 'persistent-class)
                                             (class-precedence-list class))))))

(defgeneric compute-data-table-slot-p (slot)
  (:method ((slot persistent-effective-slot-definition))
           (primitive-type-p (remove-null-and-unbound-if-or-type (slot-definition-type slot)))))

(defgeneric compute-table (slot)
  (:method ((slot persistent-direct-slot-definition))
           (awhen (remove-null-and-unbound-if-or-type (slot-definition-type slot))
             (cond ((set-type-p it)
                    (primary-table-of (find-class (second it))))
                   ((or (primitive-type-p it)
                        (persistent-class-type-p it))
                    (primary-table-of (slot-definition-class slot)))
                   (t
                    (error "Unknown slot type in slot ~A" slot)))))

  (:method ((slot persistent-effective-slot-definition))
           (table-of (last1 (direct-slots-of slot)))))

(defgeneric compute-columns (slot)
  (:method :around ((slot persistent-direct-slot-definition))
           (when (slot-definition-type slot)
             (when-bind table (table-of slot)
               (call-next-method))))

  (:method ((slot persistent-direct-slot-definition))
           (awhen (remove-null-and-unbound-if-or-type (slot-definition-type slot))
             (cond ((set-type-p it)
                    (make-columns-for-reference-slot slot))
                   ((persistent-class-type-p it)
                    (make-columns-for-reference-slot slot))
                   ((primitive-type-p it)
                    (list
                     (make-instance 'sql-column
                                    :name (rdbms-name-for (slot-definition-name slot))
                                    :type (compute-column-type it))))
                   (t
                    (error "Unknown slot type in slot ~A" slot)))))

  (:method ((slot persistent-effective-slot-definition))
           (columns-of (last1 (direct-slots-of slot)))))

(defun make-oid-columns ()
  "Creates a list of RDBMS columns that will be used to store the oid data of the objects in this table."
  (list
   (make-instance 'sql-column
                  :name +id-column-name+
                  :type +oid-id-sql-type+
                  :constraints (list (make-instance 'sql-not-null-constraint)
                                     (make-instance 'sql-primary-key-constraint)))
   (make-instance 'sql-column
                  :name +class-name-column-name+
                  :type +oid-class-name-sql-type+)))

(defun make-columns-for-reference-slot (slot)
  (list
   (make-instance 'sql-column
                  :name (rdbms-name-for (concatenate-symbol (slot-definition-name slot) "-id"))
                  :type +oid-id-sql-type+
                  :constraints (list (make-instance 'sql-unique-constraint)))
   (make-instance 'sql-column
                  :name (rdbms-name-for (concatenate-symbol (slot-definition-name slot) "-class-name"))
                  :type +oid-class-name-sql-type+)))
