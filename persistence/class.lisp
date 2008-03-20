;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Persistent class and slot meta objects

;; TODO: FIXME: maybe change those computed slots into simple defuns which are not required for instance level persistence operations (makes debugging harder?!)
;; TODO: add oid-columns for persistent-class
;; TODO: support flattenning abstract superclass slot columns into subclasses, so that abstract superclasses will not have tables
;; TODO: support flattenning subclasses into superclass and dispatch on type
;; TODO: support flattenning (1-1) associations and slots with persistent object subtype into referer's primary table

(defcclass* persistent-class (standard-class exportable)
  ((abstract
    (compute-as #f)
    :type boolean
    :documentation "An abstract persistent class cannot be instantiated but still can be used in associations and may have slots. Calling make-instance on an abstract persistent class will signal an error. On the other hand abstract classes might not have a primary table and thus handling the instances may require simpler or less SQL statements.")
   (separate-primary-table
    (compute-as (not (abstract-p -self-)))
    :type boolean
    :documentation "True if the slots of the abstract class must be stored in the non-abstract subclasses of it. This also means that the class will not have a primary table but a view instead.")
   (standard-direct-slots
    (compute-as (class-direct-slots -self-))
    :type (list standard-effective-slot-definition)
    :documentation "All computed slots that needs the direct slots should use this slot so that invalidation will work.")
   (standard-effective-slots
    (compute-as (class-slots -self-))
    :type (list standard-effective-slot-definition)
    :documentation "All computed slots that needs the effective slots should use this slot so that invalidation will work.")
   (persistent-direct-slots
    (compute-as (collect-if #L(typep !1 'persistent-direct-slot-definition) (standard-direct-slots-of -self-)))
    :type (list persistent-direct-slot-definition)
    :documentation "The list of direct slots which are defined to be persistent in this class.")
   (persistent-effective-slots
    (compute-as (collect-if #L(typep !1 'persistent-effective-slot-definition) (standard-effective-slots-of -self-)))
    :type (list persistent-effective-slot-definition)
    :documentation "The list of effective slots which are turned out to be persistent in this class.")
   (effective-slots-with-underlying-slot-access
    (compute-as (persistent-effective-slots-of -self-))
    :type (list standard-effective-slot-definition)
    :documentation "A list of slots that support the underlying-slot-value protocol.")
   (persistent-direct-super-classes
    (compute-as (collect-if #'persistent-class-p (class-direct-superclasses -self-)) )
    :type (list persistent-class)
    :documentation "The list of persistent direct sub classes.")
   (persistent-class-precedence-list
    (compute-as (list* -self- (persistent-effective-super-classes-of -self-)))
    :type (list persistent-class)
    :documentation "Similar to class-precedence-list but includes only persistent classes.")
   (persistent-effective-super-classes
    (compute-as (compute-persistent-effective-super-classes -self-))
    :type (list persistent-class)
    :documentation "The list of effective persistent super classes in class precedence order.")
   (persistent-direct-sub-classes
    (compute-as (collect-if #'persistent-class-p (class-direct-subclasses -self-)))
    :type (list persistent-class)
    :documentation "The list of persistent direct sub classes.")
   (persistent-effective-sub-classes
    (compute-as (compute-persistent-effective-sub-classes -self-))
    :type (list persistent-class)
    :documentation "The list of persistent effective sub classes in no particular order.")
   (primary-table
    (compute-as (compute-primary-table -self- -current-value-))
    :type (or null table)
    :documentation "The table which holds the oid and the data of the direct slots of this class. If the class is abstract and does not have any persistent direct slots then it will not have a primary table. A primary table if exists contains exactly one record per instance of its persistent class.")
   (primary-tables
    (compute-as (compute-primary-tables -self-))
    :type (list class-primary-table)
    :documentation "The smallest set of tables which hold all instances of this class by having exactly one record per instance. This list may contain functional nodes such as union, append according to the required SQL operation. For classes which have a primary table this list contains only that table while for other classes the list will contain some of the primary tables of the sub persistent classes.")
   (primary-view
    (compute-as (compute-primary-view -self-))
    :type (or null view)
    :documentation "When a class does not have a primary table then it is required to have a primary view which can be used to select data as if it were a primary table.")
   (primary-relation
    (compute-as (or (primary-table-of -self-) (primary-view-of -self-)))
    :type (or table view)
    :documentation "The primary relation from which instances of this class can be selected or nil.")
   (data-tables
    (compute-as (compute-data-tables -self-))
    :type (list table)
    :documentation "All the tables which hold direct data of an instance of this class. This list contains the primary tables of the super persistent classes.")
   (data-view
    (compute-as (compute-data-view -self-))
    :type (or null view)
    :documentation "This is a view which joins all data tables for this class, nil if the view would be the same as the primary table.")
   (data-relation
    (compute-as (or (data-view-of -self-) (primary-table-of -self-)))
    :type (or table view)
    :documentation "Either a view or a table which can be used to query for data slots.")
   (prefetched-slots
    (compute-as (collect-if #'prefetch-p (persistent-effective-slots-of -self-)))
    :type (list persistent-effective-slot-definition)
    :documentation "The list of effective slots which will be loaded from and stored to the database at once when loading an instance of this class. Moreover when a persistent instance is revived its prefetched slots will be loaded.")
   (non-prefetched-slots
    (compute-as (set-difference (persistent-effective-slots-of -self-) (prefetched-slots-of -self-)))
    :type (list effective-slot)
    :documentation "The list of effective slots which will be loaded and stored lazily and separately from other slots.")
   (depends-on
    (compute-as nil)
    :type (list persistent-class)
    :documentation "The list of persistent classes which must be looked at by this class when computing RDBMS meta data. This used to generate columns into other classes' primary tables.")
   (depends-on-me
    (compute-as nil)
    :type (list persistent-class)
    :documentation "The list of persistent classes which must look at this class when computing RDBMS meta data."))
  (:documentation "Persistent class is a class meta instance for classes. Standard defclass forms may be used to define persistent classes. A persistent class will have persistent slots unless marked with :persistent #f. A persistent slot should have type specification to be efficient both in storage and speed. The special type unbound must be used to mark slots which might be unbound."))

(defclass identity-preserving-class (computed-class)
  ()
  (:documentation "This class serves a very special purpose, namely being able to return the very same instance in make-instance for slot definition meta instances."))

(defcclass* persistent-slot-definition (standard-slot-definition)
  ((prefetch
    :type boolean
    :computed-in compute-as
    :documentation "Prefetched slots are loaded from and stored into the database at once. A prefetched slot must be in a table which can be accessed using a where clause matching to the id of the instance thus it must be in a data table. The default prefetched slot semantics can be overriden on a per direct slot basis.")
   (cache
    :type boolean
    :computed-in compute-as
    :documentation "All prefetched slots are cached slots but the opposite may not be true. When a cached slot is loaded it's value will be stored in the CLOS instance for fast subsequent read operations. Also whenever a cached slot is set the value will be remembered. The default cached slot semantics can be overriden on a per direct slot basis.")
   (index
    :type boolean
    :computed-in compute-as
    :documentation "True means the slot value will be indexed in the underlying RDBMS.")
   (unique
    :type boolean
    :computed-in compute-as
    :documentation "True means the slot value will be enforced to be unique among instances in the underlying RDBMS.")
   (specified-type
    (compute-as t)
    :type (or symbol cons)
    :initarg nil
    :documentation "The slot type as it was specified.")
   (canonical-type
    (compute-as (canonical-type-for (specified-type-of -self-)))
    :type (or symbol cons)
    :documentation "The canonical form of the specified type. See canonical-type-for for more details.")
   (normalized-type
    (compute-as (normalized-type-for (canonical-type-of -self-)))
    :type (or symbol cons)
    :documentation "The normalized form of the specified type. See normalized-type-for for more details.")
   (always-checked-type
    (compute-as (compute-always-checked-type -self-))
    :type list
    :documentation "When type-check is :always then this type will be checked whenever a new value is set during the transaction. This type may be different from the specified type.")
   (type-check
    :type (member :always :on-commit)
    :computed-in compute-as
    :documentation "On commit type check means that during the transaction the slot may have null and/or unbound value and the type check will be done when the transaction commits."))
  (:documentation "Base class for both persistent direct and effective slot definitions."))

(defcclass* persistent-direct-slot-definition (persistent-slot-definition standard-direct-slot-definition)
  ((store-in-primary-table
    (compute-as #f)
    :type boolean
    :documentation "Specifies that the slot must be stored in the owner class'es primary table and should not be stored in multiple subclasses' primary tables.")
   (specified-type
    :initarg :type
    :documentation "The slot type as it was originally specified in the defclass form."))
  (:metaclass identity-preserving-class)
  (:documentation "Class for persistent direct slot definitions."))

(defcclass* persistent-effective-slot-definition (persistent-slot-definition standard-effective-slot-definition)
  ((direct-slots
    :type (list persistent-direct-slot-definition)
    :documentation "The list of direct slots definitions used to compute this effective slot during the class finalization procedure.")
   (primary-class
    (compute-as (compute-primary-class -self-))
    :type persistent-class
    :documentation "The persistent class which owns the primary table where this slot will be stored.")
   (table
    (compute-as (compute-table -self-))
    :type table
    :documentation "The RDBMS table which will be queried or updated to get and set the data of this slot.")
   (column-names
    (compute-as (compute-column-names -self-))
    :type list
    :documentation "The list of RDBMS column names to which this slot will be mapped.")
   (columns
    (compute-as (compute-columns -self-))
    :type (list sql-column)
    :documentation "The list of RDBMS columns which will be queried or updated to get and set the data of this slot.")
   (id-column
    (compute-as (bind ((type (canonical-type-of -self-)))
                  (if (or (persistent-class-type-p* type)
                          (set-type-p* type))
                      (first (columns-of -self-)))))
    :type sql-column
    :documentation "This is the id column of the oid reference when appropriarte for the slot type.")
   (mapping
    (compute-as (compute-slot-mapping -self-))
    :type mapping
    :documentation "The RDBMS mapping")
   (column-types
    (compute-as (awhen (mapping-of -self-) (rdbms-types-of it)))
    :type list
    :documentation "List of RDBMS types to which this slot is mapped.")
   (reader
    (compute-as (compute-slot-reader -self-))
    :type (or null function)
    :documentation "A function which transforms RDBMS values to the corresponding lisp value. This is present only for data table slots.")
   (writer
    (compute-as (compute-slot-writer -self-))
    :type (or null function)
    :documentation "A function which transforms a lisp value to the corresponding RDBMS values. This is present only for data table slots.")
   (primary-table-slot
    (compute-as (compute-primary-table-slot-p -self-))
    :type boolean
    :documentation "True means the slot can be loaded from the primary table of its class with a where clause matching to the instance's oid.")
   (data-table-slot
    (compute-as (compute-data-table-slot-p -self-))
    :type boolean
    :documentation "True means the slot can be loaded from one of the data tables of its class with a where clause matching to the instance's oid.")
   (prefetch
    (compute-as (data-table-slot-p -self-))
    :documentation "The prefetched option is inherited among direct slots according to the class precedence list. If no direct slot has prefetched specification then the default behaviour is to prefetch data tabe slots.")
   (cache
    (compute-as (or (prefetch-p -self-)
                    (persistent-class-type-p* (canonical-type-of -self-))))
    :documentation "The cached option is inherited among direct slots according to the class precedence list. If no direct slot has cached specification then the default behaviour is to cache prefetched slots and single instance references.")
   (index
    (compute-as #f)
    :documentation "The index option is inherited among direct slots according to the class precedence list with defaulting to false.")
   (unique
    (compute-as #f)
    :documentation "The unique option is inherited among direct slots according to the class precedence list with defaulting to false.")
   (specified-type
    (compute-as (cons 'and (mapcar 'specified-type-of (direct-slots-of -self-))))
    :documentation "The types of the direct slots combined with the compount type specifier 'and'.")
   (type-check
    (compute-as (if (persistent-class-type-p* (canonical-type-of -self-))
                    :on-commit
                    :always))
    :documentation "The type check option is inherited among direct slots according to the class precedence list with defaulting to :always. for primitive types and :on-commit for class types."))
  (:documentation "Class for persistent effective slot definitions."))

(eval-always
  (mapc #L(pushnew !1 *allowed-slot-definition-properties*) '(:persistent :store-in-primary-table :prefetch :cache :index :unique :type-check)))

(defcclass* class-primary-table (table)
  ((persistent-class
    :type persistent-class
    :documentation "The persistent class for which this table is the primary table.")
   (oid-columns
    (compute-as (oid-mode-ecase
                  (:class-name (list (id-column-of -self-) (class-name-column-of -self-)))
                  (:class-id (list (id-column-of -self-) (class-id-column-of -self-)))
                  (:merge (list (id-column-of -self-)))))
    :type (list sql-column)
    :documentation "The list of RDBMS columns corresponding to the oid of this table.")
   (id-column
    (compute-as (find +oid-id-column-name+ (columns-of -self-) :key 'cl-rdbms::name-of))
    :type sql-column
    :documentation "The RDBMS column of the corresponding oid slot.")
   (class-name-column
    (compute-as (find +oid-class-name-column-name+ (columns-of -self-) :key 'cl-rdbms::name-of))
    :type sql-column
    :documentation "The RDBMS column of the corresponding oid slot.")
   (class-id-column
    (compute-as (find +oid-class-id-column-name+ (columns-of -self-) :key 'cl-rdbms::name-of))
    :type sql-column
    :documentation "The RDBMS column of the corresponding oid slot."))
  (:documentation "This is a special table related to a persistent class."))

(defmethod describe-object ((instance persistent-class) stream)
  (call-next-method)
  (aif (primary-table-of instance)
       (progn
         (princ "The primary table is the following: ")
         (describe-object it stream))
       (princ (format nil "The primary tables are: ~A" (primary-tables-of instance)) stream)))

(def print-object persistent-slot-definition
  (princ (slot-definition-name self)))

;;;;;;;;;;;;;
;;; defpclass

(defmethod expand-defpclass-form ((metaclass persistent-class) defclass-macro name superclasses slots options)
  `(,defclass-macro ,name ,superclasses ,slots ,@options))

;;;;;;;;;;
;;; Export

(defmethod export-to-rdbms ((class persistent-class))
  (oid-mode-ecase
    (:class-name)
    ((:class-id :merge)
     (let ((class-name (class-name class)))
       (setf (class-id->class-name (class-name->class-id class-name)) class-name))))
  (ensure-finalized class)
  (mapc #'ensure-exported
        (persistent-effective-super-classes-of class))
  (mapc #'ensure-exported
        (collect-if #L(typep !1 'persistent-association)
                    (depends-on-of class)))
  (awhen (primary-table-of class)
    (ensure-exported it))
  (awhen (primary-view-of class)
    (mapc #'ensure-exported (cdr (primary-tables-of class)))
    (ensure-exported it))
  (awhen (data-view-of class)
    (ensure-exported it)))

;;;;;;;;;;;
;;; Mapping

(defmethod compute-rdbms-types* ((mapped-type persistent-class) normalized-type)
  (compute-rdbms-types* (class-name mapped-type) normalized-type))

(defmethod compute-reader* ((mapped-type persistent-class) normalized-type)
  (compute-reader* (class-name mapped-type) normalized-type))


(defmethod compute-writer* ((mapped-type persistent-class) normalized-type)
  (compute-writer* (class-name mapped-type) normalized-type))


;;;;;;;;;;;;
;;; Computed

(defgeneric compute-always-checked-type (slot)
  (:method ((slot persistent-slot-definition))
           (bind ((type (canonical-type-of slot)))
             (if (and (eq :on-commit (type-check-of slot))
                      (not (slot-definition-initfunction slot)))
                 `(or unbound ,type)
                 type))))

(defgeneric compute-persistent-effective-super-classes (class)
  (:method ((class persistent-class))
           (delete-duplicates
            (append (persistent-direct-super-classes-of class)
                    (iter (for super-class in (persistent-direct-super-classes-of class))
                          (appending (persistent-effective-super-classes-of super-class)))))))

(defgeneric compute-persistent-effective-sub-classes (class)
  (:method ((class persistent-class))
           (delete-duplicates
            (append (persistent-direct-sub-classes-of class)
                    (iter (for sub-class in (persistent-direct-sub-classes-of class))
                          (appending (persistent-effective-sub-classes-of sub-class)))))))

(defgeneric compute-slot-mapping (slot)
  (:method ((slot persistent-effective-slot-definition))
    (compute-mapping (always-checked-type-of slot))))

(defgeneric compute-slot-reader (slot)
  (:method ((slot persistent-effective-slot-definition))
    (coerce (reader-of (mapping-of slot)) 'function)) )

(defgeneric compute-slot-writer (slot)
  (:method ((slot persistent-effective-slot-definition))
    (coerce (writer-of (mapping-of slot)) 'function)))

(defgeneric compute-primary-table (class current-table)
  (:method ((class persistent-class) current-table)
    (ensure-finalized class)
    (flet ((primary-table-columns-for-class (class)
             ;; those mappends may collect the same columns several times (compare by identity)
             (delete-duplicates
              (append
               (mappend #L(when (primary-table-slot-p !1)
                            (columns-of !1))
                        (persistent-effective-slots-of class))
               (mappend #L(when (eq class (primary-class-of !1))
                            (columns-of !1))
                        (mappend #L(persistent-effective-slots-of !1)
                                 (collect-if #L(typep !1 'persistent-class) (depends-on-of class))))))))
      (when (or (not (abstract-p class))
                (primary-table-columns-for-class class))
        (or current-table
            (make-instance 'class-primary-table
                           :name (rdbms-name-for (class-name class) :table)
                           :persistent-class class
                           :columns (compute-as
                                      (append
                                       (make-oid-columns)
                                       (primary-table-columns-for-class class)))))))))

(defgeneric compute-primary-tables (class)
  (:method ((class persistent-class))
    (labels ((primary-classes-of (class)
               (if (primary-table-of class)
                   (list class)
                   (iter (for sub-class in (persistent-direct-sub-classes-of class))
                         (appending (primary-classes-of sub-class))))))
      (bind ((primary-classes (primary-classes-of class))
             (primary-class-sub-classes (mapcar #'persistent-effective-sub-classes-of primary-classes))
             (primary-tables (mapcar #'primary-table-of primary-classes)))
        (when primary-class-sub-classes
          (if (eq (length (reduce #'union primary-class-sub-classes))
                  (length (reduce #'append primary-class-sub-classes)))
              (cons :append primary-tables)
              (cons :union primary-tables)))))))

(defgeneric compute-primary-view (class)
  (:method ((class persistent-class))
    (unless (primary-table-of class)
      (when-bind primary-tables (primary-tables-of class)
        (bind ((columns (append +oid-column-names+
                                (mappend (lambda (slot)
                                           (columns-of
                                            (find-slot (persistent-class-of (first (cdr primary-tables)))
                                                       (slot-definition-name slot))))
                                         (collect-if (lambda (slot)
                                                       (bind ((canonical-type (canonical-type-of slot)))
                                                         (or (primitive-type-p* canonical-type)
                                                             (persistent-class-type-p canonical-type))))
                                                     (persistent-direct-slots-of class))))))
          (make-instance 'view
                         :name (rdbms-name-for (concatenate-symbol (class-name class) "-view") :view)
                         :columns columns
                         :query (sql-set-operation-expression :set-operation :union :all (eq :append (first primary-tables))
                                                              :subqueries
                                                              (mapcar (lambda (table)
                                                                        (sql-subquery :query
                                                                                      (sql-select :columns columns
                                                                                                  :tables (list (name-of table)))))
                                                                      (cdr primary-tables)))))))))

(defgeneric compute-data-tables (class)
  (:method ((class persistent-class))
    (delete-if #'null
               (mapcar #'primary-table-of
                       (list* class (persistent-effective-super-classes-of class))))))

(defgeneric compute-data-view (class)
  (:method ((class persistent-class))
    (when (> (length (data-tables-of class)) 1)
      (make-instance 'view
                     :name (rdbms-name-for (concatenate-symbol (class-name class) "-data") :view)
                     :columns nil
                     :query (sql-select :columns (list (sql-all-columns))
                                        :tables (list (reduce #L(sql-joined-table :kind :inner
                                                                                  :using +oid-column-names+
                                                                                  :left !1
                                                                                  :right !2)
                                                              (mapcar #L(sql-identifier :name (name-of !1)) (data-tables-of class)))))))))

(defgeneric compute-primary-table-slot-p (slot)
  (:method ((slot persistent-effective-slot-definition))
    (and (not (some #'primary-table-slot-p (persistent-effective-super-slot-precedence-list-of slot)))
         (data-table-slot-p slot)
         (eq (primary-class-of slot) (slot-definition-class slot)))))

(defgeneric compute-data-table-slot-p (slot)
  (:method ((slot persistent-effective-slot-definition))
    (bind ((type (canonical-type-of slot)))
      (and (subtypep (slot-definition-class slot) (primary-class-of slot))
           (or (primitive-type-p* type)
               (persistent-class-type-p* type))))))

(defgeneric compute-primary-class (slot)
  (:method ((slot persistent-effective-slot-definition))
    (or (some #'primary-class-of (persistent-effective-super-slot-precedence-list-of slot))
        (bind ((class (slot-definition-class slot))
               (canonical-type (canonical-type-of slot)))
          (cond ((set-type-p* canonical-type)
                 (find-class (set-type-class-for canonical-type)))
                ((or (primitive-type-p* canonical-type)
                     (persistent-class-type-p* canonical-type))
                 class)
                (t
                 (error "Unknown type ~A in slot ~A" (specified-type-of slot) slot)))))))

(defgeneric compute-table (slot)
  (:method ((slot persistent-effective-slot-definition))
    (primary-table-of (primary-class-of slot))))

(defgeneric compute-column-names (slot)
  (:method ((slot persistent-effective-slot-definition))
    (mapcar 'rdbms::name-of (columns-of slot))))

(defgeneric compute-columns (slot)
  (:method ((slot persistent-effective-slot-definition))
    (bind ((precedence-list (persistent-effective-super-slot-precedence-list-of slot)))
      ;; TODO: multiple inheritance with slot storage location merging is not yet supported
      (assert (<= (length (delete nil (delete-duplicates (mapcar 'columns-of precedence-list)))) 1) nil "There must be at most one storage location for ~A" slot)
      (or (some #'columns-of precedence-list)
          (bind ((class (slot-definition-class slot))
                 (name (slot-definition-name slot))
                 (class-name (class-name class))
                 (type (canonical-type-of slot))
                 (mapping (mapping-of slot))
                 (rdbms-types (column-types-of slot)))
            (when type
              (cond ((set-type-p* type)
                     (make-columns-for-reference-slot class-name (strcat name "-for-" class-name)))
                    ((persistent-class-type-p* type)
                     (append
                      (when (tagged-p mapping)
                        (list (make-tag-column mapping name)))
                      (make-columns-for-reference-slot class-name name)))
                    ((primitive-type-p* type)
                     (append
                      (when (tagged-p mapping)
                        (list (make-tag-column mapping name)))
                      (list
                       (make-instance 'column
                                      :name (rdbms-name-for name :column)
                                      :type (if (tagged-p mapping)
                                                (second rdbms-types)
                                                (first rdbms-types))
                                      ;; TODO: add null constraint if type-check is :always (and (not (subytpep 'null type))
                                      ;;                                                         (not (subytpep 'unbound type)))
                                      :constraints (if (unique-p slot)
                                                       (list (sql-unique-constraint)))
                                      :index (if (and (index-p slot)
                                                      (not (unique-p slot)))
                                                 (sql-index :name
                                                            (rdbms-name-for (concatenate-symbol name "-on-" class-name "-idx")
                                                                            :index)))))))
                    (t
                     (error "Unknown type ~A in slot ~A" type slot)))))))))

;;;;;;;;;;;
;;; Utility

(def (special-variable :documentation "A mapping from persistent class names to persistent instances.")
    *persistent-classes* (make-hash-table))

(def (function e) find-persistent-class (name)
  (gethash name *persistent-classes*))

(def function find-persistent-class* (name-or-class)
  (etypecase name-or-class
    (symbol (find-persistent-class name-or-class))
    (persistent-class name-or-class)))

(def function (setf find-persistent-class) (new-value name)
  (setf (gethash name *persistent-classes*) new-value))

(def function finalize-persistent-classes ()
  (iter (for (class-name class) :in-hashtable *persistent-classes*)
        (ensure-all-computed-slots-are-valid class)
        (dolist (slot (class-slots class))
          (ensure-all-computed-slots-are-valid slot))))

(def function persistent-class-p (class)
  (typep class 'persistent-class))

(def function persistent-class-name-p (name)
  (and name
       (symbolp name)
       (persistent-class-p (find-class name #f))))

(def function persistent-slot-p (slot)
  (typep slot 'persistent-slot-definition))

(def function slot-definition-class (slot)
  "Returns the class to which the given slot belongs."
  #+sbcl(slot-value slot 'sb-pcl::%class)
  #-sbcl(not-yet-implemented))

(def function persistent-effective-super-slot-precedence-list-of (slot)
  (bind ((slot-name (slot-definition-name slot))
         (slot-class (slot-definition-class slot)))
    (ensure-finalized slot-class)
    (iter (for class in (persistent-effective-super-classes-of slot-class))
          (ensure-finalized class)
          (aif (find slot-name (persistent-effective-slots-of class) :key #'slot-definition-name)
               (collect it)))))

(def function slot-accessor-p (name)
  (and (symbolp name)
       (effective-slots-for-accessor name)))

(def function effective-slots-for-accessor (name)
  (iter (for (class-name class) in-hashtable *persistent-classes*)
        (awhen (find name (persistent-direct-slots-of class)
                     :key #'slot-definition-readers
                     :test #'member)
          (ensure-finalized class)
          (collect (prog1 (find-slot class (slot-definition-name it))
                     (assert it))))))

(def function effective-slots-for-slot-name (slot-name)
  (iter (for (class-name class) in-hashtable *persistent-classes*)
        (for slot = (find-slot (ensure-finalized class) slot-name))
        (when slot (collect slot))))

(def function make-oid-columns ()
  "Creates a list of RDBMS columns that will be used to store the oid data of the instances in this table."
  (append
   (list (make-instance 'column
                        :name +oid-id-column-name+
                        :type +oid-id-sql-type+
                        :constraints (list (sql-not-null-constraint)
                                           (sql-primary-key-constraint))))
   (oid-mode-ecase
    (:class-name
     (list (make-instance 'column
                          :name +oid-class-name-column-name+
                          :type +oid-class-name-sql-type+)))
    (:class-id
     (list (make-instance 'column
                          :name +oid-class-id-column-name+
                          :type +oid-class-id-sql-type+)))
    (:merge))))

(def function make-columns-for-reference-slot (class-name column-name)
  (bind ((id-column-name (rdbms-name-for (concatenate-symbol column-name "-id")) :column)
         (id-index-name (rdbms-name-for (concatenate-symbol column-name "-id-on-" class-name "-idx") :index)))
    (append
     (list (make-instance 'column
                          :name id-column-name
                          :type +oid-id-sql-type+
                          :index (sql-index :name id-index-name)))
     (oid-mode-ecase
      (:class-name
       (list (make-instance 'column
                            :name (rdbms-name-for (concatenate-symbol column-name "-class-name") :column)
                            :type +oid-class-name-sql-type+)))
      (:class-id
       (list (make-instance 'column
                            :name (rdbms-name-for (concatenate-symbol column-name "-class-id") :column)
                            :type +oid-class-id-sql-type+)))
      (:merge)))))

(def function make-tag-column (mapping name)
  (bind ((type (first (rdbms-types-of mapping)))
         (nullable (first (nullable-types-of mapping))))
    (make-instance 'column
                   :name (rdbms-name-for (concatenate-symbol name "-tag") :column)
                   :type type
                   :constraints (unless nullable
                                  (list (sql-not-null-constraint)))
                   :default-value (if nullable
                                      :null
                                      0))))

(def function tag-column-of (slot)
  (when (tagged-p (mapping-of slot))
    (first (columns-of slot))))

(defmethod matches-type* (value (type symbol))
  (and (typep value type)
       (or (not (persistent-class-type-p type))
           (every (lambda (slot)
                    (bind ((type (canonical-type-of slot))
                           (class (class-of value)))
                      (unless (funcall *matches-type-cut-function* value type)
                        (if (slot-boundp-using-class class value slot)
                            (bind ((slot-value (slot-value-using-class class value slot)))
                              (aprog1 (matches-type* slot-value type)
                                (unless it
                                  (error (make-condition 'instance-slot-type-violation :instance value :slot slot)))))
                            (not (unbound-subtype-p type))))))
                  (persistent-effective-slots-of type)))))
