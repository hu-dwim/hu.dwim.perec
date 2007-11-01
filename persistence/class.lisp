;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Persistent class and slot meta instances

;; TODO: add oid-columns for persistent-class
;; TODO: support flattenning abstract superclass slots into subclasses
;; TODO: support flattenning subclasses into superclass and dispatch on type
;; TODO: support flattenning (1-1) associations and slots with persistent object subtype into referer's primary table

(defcclass* persistent-class (standard-class exportable)
  ((abstract
    (compute-as #f)
    :type boolean
    :documentation "An abstract persistent class cannot be instantiated but still can be used in associations and may have slots. Calling make-instance on an abstract persistent class will signal an error. On the other hand abstract classes might not have a primary table and thus handling the instances may require simpler or less SQL statements.")
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
    (compute-as (collect-if #'prefetch-p (persistent-effective-slots-of -self-)))
    :type (list persistent-effective-slot-definition)
    :documentation "The list of effective slots which will be loaded from and stored to the database at once when loading an instance of this class. Moreover when a persistent instance is revived its prefetched slots will be loaded.")
   (non-prefetched-slots
    (compute-as (remove-if #'prefetch-p (persistent-effective-slots-of -self-)))
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
  ((always-checked-type
    (compute-as (compute-always-checked-type -self-))
    :type list
    :documentation "When type-check is :always then this type will be checked whenever a new value is set during the transaction.")
   (prefetch
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
   (type-check
    :type (member :always :on-commit)
    :computed-in compute-as
    :documentation "On commit type check means that during the transaction the slot may have null and/or unbound value and the type check will be done when the transaction commits."))
  (:documentation "Base class for both persistent direct and effective slot definitions."))

(defcclass* persistent-direct-slot-definition
    (persistent-slot-definition standard-direct-slot-definition)
  ()
  (:metaclass identity-preserving-class)
  (:documentation "Class for persistent direct slot definitions."))

(defcclass* persistent-effective-slot-definition
    (persistent-slot-definition standard-effective-slot-definition)
  ((normalized-type
    (compute-as (normalized-type-for (slot-definition-type -self-)))
    :type list)
   (direct-slots
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
   (columns
    (compute-as (compute-columns -self-))
    :type (list sql-column)
    :documentation "The list of RDBMS columns which will be queried or updated to get and set the data of this slot.")
   (id-column
    (compute-as (bind ((type (normalized-type-of -self-)))
                  (if (or (persistent-class-type-p type)
                          (set-type-p type))
                      (first (columns-of -self-)))))
    :type sql-column
    :documentation "This is the id column of the oid reference when appropriarte for the slot type.")
   (reader
    (compute-as (compute-reader -self- (always-checked-type-of -self-)))
    :type (or null function)
    :documentation "A one parameter function which transforms RDBMS data received as a list to the corresponding lisp instance. This is present only for data table slots.")
   (writer
    (compute-as (compute-writer -self- (always-checked-type-of -self-)))
    :type (or null function)
    :documentation "A one parameter function which transforms a lisp instance to the corresponding RDBMS data. This is present only for data table slots.")
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
    :documentation "The prefetched option is inherited among direct slots according to the class precedence list. If no direct slot has prefetched specification then the default behaviour is to prefetch data tabe slot.")
   (cache
    (compute-as (or (prefetch-p -self-)
                    (persistent-class-type-p (normalized-type-of -self-))))
    :documentation "The cached option is inherited among direct slots according to the class precedence list. If no direct slot has cached specification then the default behaviour is to cache prefetched slots and single instance references.")
   (index
    (compute-as #f)
    :documentation "The index option is inherited among direct slots according to the class precedence list with defaulting to false.")
   (unique
    (compute-as #f)
    :documentation "The unique option is inherited among direct slots according to the class precedence list with defaulting to false.")
   (type-check
    (compute-as (if (persistent-class-type-p (normalized-type-of -self-))
                    :on-commit
                    :always))
    :documentation "The type check option is inherited among direct slots according to the class precedence list with defaulting to :always. for primitive types and :on-commit for class types."))
  (:documentation "Class for persistent effective slot definitions."))

(eval-always
  (mapc #L(pushnew !1 *allowed-slot-definition-properties*) '(:persistent :prefetch :cache :index :unique :type-check)))

(defcclass* class-primary-table (table)
  ((oid-columns
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

(defprint-object (slot persistent-slot-definition)
  (princ (slot-definition-name slot)))

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
    (ensure-exported it)))

;;;;;;;;;;;;
;;; Computed

(defgeneric compute-always-checked-type (slot)
  (:method ((slot persistent-slot-definition))
           (bind ((type (slot-definition-type slot)))
             (if (and (eq :on-commit (type-check-of slot))
                      (not (slot-definition-initfunction slot)))
                 `(or unbound ,type)
                 type))))

(defgeneric compute-persistent-effective-super-classes (class)
  (:method ((class persistent-class))
           (remove-if #L(eq class !1)
                      (collect-if #L(typep !1 'persistent-class)
                                  (class-precedence-list class)))))

(defgeneric compute-persistent-effective-sub-classes (class)
  (:method ((class persistent-class))
           (delete-duplicates
            (append (persistent-direct-sub-classes-of class)
                    (iter (for sub-class in (persistent-direct-sub-classes-of class))
                          (appending (persistent-effective-sub-classes-of sub-class)))))))

(defun mapped-type-for (type)
  (if (persistent-class-type-p type)
      type
      (find-if #L(cond ((or (eq type !1)
                            (and (listp type)
                                 (eq (first type) !1)))
                        #t)
                       ((eq 'member !1)
                        (and (listp type)
                             (eq 'member (first type))))
                       (t
                        (subtypep type !1)))
               *mapped-type-precedence-list*)))

(defun normalized-type-for (type)
  (let ((*canonical-types* (append *mapped-type-precedence-list* *canonical-types*)))
    (canonical-type-for
     `(and (not null)
           (not unbound)
           ,type))))

(defun compute-column-type (type)
  "Returns the RDBMS type for the given type."
  (let ((normalized-type (normalized-type-for type)))
    (compute-column-type* (mapped-type-for normalized-type) normalized-type)))

(defgeneric compute-column-type* (type type-specification)
  (:method (type type-specification)
           (declare (ignore type-specification))
           (error "Cannot map type ~A to RDBMS type" type)))

(defun column-count-for (normalized-type unbound-and-null-subtype-p)
  (+ (cond ((persistent-class-type-p normalized-type)
            (oid-mode-ecase
              ((:class-name :class-id) 2)
              (:merge 1)))
           ((primitive-type-p normalized-type)
            1)
           (t (error "Cannot map type ~A to a writer" normalized-type)))
     (if unbound-and-null-subtype-p
         1
         0)))

(defun compute-transformer (transformer-type slot type)
  "Maps a type to a one parameter lambda which will be called with the received RDBMS values."
  (flet ((wrapper-function-for (symbol-or-function)
           (if (functionp symbol-or-function)
               symbol-or-function
               (concatenate-symbol (find-package :cl-perec) symbol-or-function "-" transformer-type)))
         (identity-wrapper (slot type function column-number)
           (declare (ignorable slot type column-number))
           function))
    (bind ((normalized-type (normalized-type-for type))
           (mapped-type (mapped-type-for normalized-type))
           (unbound-subtype-p (unbound-subtype-p type))
           (null-subtype-p (and (not (null-subtype-p mapped-type))
                                (null-subtype-p type)))
           (column-count (column-count-for normalized-type (and unbound-subtype-p null-subtype-p)))
           ((values wrapper-1 wrapper-2)
            (cond ((and unbound-subtype-p
                        null-subtype-p)
                   (values 'unbound-or-null #'identity-wrapper))
                  ((and unbound-subtype-p
                        (not null-subtype-p))
                   (values 'unbound (if (null-subtype-p mapped-type)
                                        #'identity-wrapper
                                        'non-null)))
                  ((and (not unbound-subtype-p)
                        null-subtype-p)
                   (values 'non-unbound 'null))
                  ((and (not unbound-subtype-p)
                        (not null-subtype-p))
                   (values 'non-unbound (if (null-subtype-p mapped-type)
                                            #'identity-wrapper
                                            'non-null))))))
      (values
       (funcall (wrapper-function-for wrapper-1)
                slot
                type
                (funcall (wrapper-function-for wrapper-2)
                         slot
                         type
                         (funcall (ecase transformer-type
                                    (reader 'compute-reader*)
                                    (writer 'compute-writer*))
                                  mapped-type
                                  normalized-type)
                         column-count)
                column-count)
       wrapper-1
       wrapper-2
       column-count))))

(defgeneric compute-reader (slot type)
  (:documentation "Maps a type to a one parameter lambda which will be called with the received RDBMS values.")

  (:method (slot type)
    (compute-transformer 'reader slot type)))

(defgeneric compute-reader* (type type-specification)
  (:method (type type-specification)
           (declare (ignore type-specification))
           (error "Cannot map type ~A to a reader" type))

  (:method ((type symbol) type-specification)
           (declare (ignore type-specification))
           (if (persistent-class-type-p type)
               (if cl-perec-system:*load-as-production-p*
                   #'object-reader
                   'object-reader)
               (call-next-method)))

  (:method ((type persistent-class) type-specification)
           (declare (ignore type-specification))
           (if cl-perec-system:*load-as-production-p*
               #'object-reader
               'object-reader)))

(defgeneric compute-writer (slot type)
  (:documentation "Maps a type to a one parameter lambda which will be called with the slot value.")
  
  (:method (slot type)
    (compute-transformer 'writer slot type)))

(defgeneric compute-writer* (type type-specification)
  (:method (type type-specification)
           (declare (ignore type-specification))
           (error "Cannot map type ~A to a writer" type))

  (:method ((type symbol) type-specification)
           (declare (ignore type-specification))
           (if (persistent-class-type-p type)
               (if cl-perec-system:*load-as-production-p*
                   #'object-writer
                   'object-writer)
               (call-next-method)))

  (:method ((type persistent-class) type-specification)
           (declare (ignore type-specification))
           (if cl-perec-system:*load-as-production-p*
               #'object-writer
               'object-writer)))

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
                     (cons 'append primary-tables)
                     (cons 'union primary-tables)))))))

(defgeneric compute-data-tables (class)
  (:method ((class persistent-class))
           (delete-if #'null
                      (mapcar #'primary-table-of
                              (list* class (persistent-effective-super-classes-of class))))))

(defgeneric compute-primary-table-slot-p (slot)
  (:method ((slot persistent-effective-slot-definition))
           (and (not (some #'primary-table-slot-p (persistent-effective-super-slot-precedence-list-of slot)))
                (data-table-slot-p slot)
                (eq (primary-class-of slot) (slot-definition-class slot)))))

(defgeneric compute-data-table-slot-p (slot)
  (:method ((slot persistent-effective-slot-definition))
           (bind ((type (normalized-type-of slot)))
             (and (subtypep (slot-definition-class slot) (primary-class-of slot))
                  (or (primitive-type-p type)
                      (persistent-class-type-p type))))))

(defgeneric compute-primary-class (slot)
  (:method ((slot persistent-effective-slot-definition))
           (or (some #'primary-class-of (persistent-effective-super-slot-precedence-list-of slot))
               (awhen (normalized-type-of slot)
                 (cond ((set-type-p it)
                        (find-class (set-type-class-for it)))
                       ((or (primitive-type-p it)
                            (persistent-class-type-p it))
                        (slot-definition-class slot))
                       (t
                        (error "Unknown type ~A in slot ~A" (slot-definition-type slot) slot)))))))

(defgeneric compute-table (slot)
  (:method ((slot persistent-effective-slot-definition))
           (primary-table-of (primary-class-of slot))))

(defgeneric compute-columns (slot)
  (:method ((slot persistent-effective-slot-definition))
           (or (some #'columns-of (persistent-effective-super-slot-precedence-list-of slot))
               (bind ((name (slot-definition-name slot))
                      (type (slot-definition-type slot))
                      (normalized-type (normalized-type-of slot))
                      (mapped-type (mapped-type-for normalized-type))
                      (complex-type-p (and (unbound-subtype-p type)
                                           (null-subtype-p type)
                                           (not (null-subtype-p mapped-type))))
                      (class (slot-definition-class slot))
                      (class-name (class-name class)))
                 (when normalized-type
                   (cond ((set-type-p normalized-type)
                          (make-columns-for-reference-slot class-name
                                                           (strcat name "-for-" class-name)))
                         ((persistent-class-type-p normalized-type)
                          (append
                           (when complex-type-p
                             (list (make-bound-column name)))
                           (make-columns-for-reference-slot class-name name)))
                         ((primitive-type-p normalized-type)
                          (append
                           (when complex-type-p
                             (list (make-bound-column name)))
                           (list
                            (make-instance 'column
                                           :name (rdbms-name-for name :column)
                                           :type (compute-column-type type)
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
                          (error "Unknown type ~A in slot ~A" type slot))))))))

;;;;;;;;
;;; Type

(defun null-inclusive-type-p (type)
  (bind ((mapped-type (mapped-type-for type)))
    (or (subtypep mapped-type 'boolean)
        (subtypep mapped-type 'symbol))))

(defun primitive-type-p (type)
  "Accepts types such as boolean, integer, string, double, etc."
  (and (not (persistent-class-type-p type))
       (not (set-type-p type))
       (not (unbound-subtype-p type))
       (or (not (null-subtype-p type))
           (null-inclusive-type-p type))))

(defun primitive-type-p* (type)
  "Same as primitive-type-p but also accepts values such as (or unbound integer), (or null string), (or unbound null boolean), etc."
  (primitive-type-p (normalized-type-for type)))

(defun persistent-class-type-p (type)
  "Returns true for persistent class types."
  (and (subtypep type 'persistent-object)
       (not (unbound-subtype-p type))
       (not (null-subtype-p type))))

(defun persistent-class-type-p* (type)
  "Same as persistent-class-type-p but also accepts values such as (or unbound persistent-object), (or null persistent-object), (or unbound null persistent-object) etc."
  (persistent-class-type-p (normalized-type-for type)))

(defun set-type-p (type)
  "Returns true for persistent set types."
  (and (not (subtypep type 'list))
       (or (subtypep type '(set persistent-object))
           (subtypep type '(disjunct-set persistent-object))
           (subtypep type '(ordered-set persistent-object)))))

(defun ordered-set-type-p (type)
  (and (not (subtypep type 'list))
       (subtypep type '(ordered-set persistent-object))))

(defun set-type-class-for (type)
  ;; FIXME: class inheritance merges the type and it's fragile anyway
  (second
   (find-if #L(member (first (ensure-list !1))
                      '(set disjunct-set ordered-set))
            (ensure-list type))))

(defun unbound-subtype-p (type)
  (and (not (eq 'member type))
       (subtypep 'unbound type)))

(defun null-subtype-p (type)
  (and (not (eq 'member type))
       (subtypep 'null type)))

(defun complex-type-p (type)
  (bind (((values normalized-type null-subtype-p unbound-subtype-p) (destructure-type type)))
    (declare (ignore normalized-type))
    (and null-subtype-p unbound-subtype-p)))

(defmethod matches-type* (value (type symbol))
  (and (typep value type)
       (or (not (persistent-class-type-p type))
           (every (lambda (slot)
                    (bind ((type (normalized-type-of slot))
                           (class (class-of value)))
                      (unless (funcall *matches-type-cut-function* value type)
                        (if (slot-boundp-using-class class value slot)
                            (bind ((slot-value (slot-value-using-class class value slot)))
                              (aprog1 (matches-type* slot-value type)
                                (unless it
                                  (error (make-condition 'instance-slot-type-violation :instance value :slot slot)))))
                            (not (unbound-subtype-p type))))))
                  (persistent-effective-slots-of type)))))

;;;;;;;;;;;
;;; Utility

(defparameter *persistent-classes* (make-hash-table)
  "A mapping from persistent class names to persistent instances.")

(defun find-persistent-class (name)
  (gethash name *persistent-classes*))

(defun find-persistent-class* (name-or-class)
  (etypecase name-or-class
    (symbol (find-persistent-class name-or-class))
    (persistent-class name-or-class)))

(defun (setf find-persistent-class) (new-value name)
  (setf (gethash name *persistent-classes*) new-value))

(def function finalize-persistent-classes ()
  (iter (for (class-name class) :in-hashtable *persistent-classes*)
        (ensure-all-computed-slots-are-valid class)
        (dolist (slot (class-slots class))
          (ensure-all-computed-slots-are-valid slot))))

(defun persistent-class-p (class)
  (typep class 'persistent-class))

(defun persistent-class-name-p (name)
  (and name
       (symbolp name)
       (persistent-class-p (find-class name #f))))

(defun persistent-slot-p (slot)
  (typep slot 'persistent-slot-definition))

(defun slot-definition-class (slot)
  "Returns the class to which the given slot belongs."
  #+sbcl(slot-value slot 'sb-pcl::%class)
  #-sbcl(not-yet-implemented))

(defun persistent-effective-super-slot-precedence-list-of (slot)
  (bind ((slot-name (slot-definition-name slot))
         (slot-class (slot-definition-class slot)))
    (ensure-finalized slot-class)
    (iter (for class in (persistent-effective-super-classes-of slot-class))
          (ensure-finalized class)
          (aif (find slot-name (persistent-effective-slots-of class) :key #'slot-definition-name)
               (collect it)))))

(defun slot-accessor-p (name)
  (and (symbolp name)
       (effective-slots-for-accessor name)))

(defun effective-slots-for-accessor (name)
  (iter (for (class-name class) in-hashtable *persistent-classes*)
        (awhen (find name (persistent-direct-slots-of class)
                     :key #'slot-definition-readers
                     :test #'member)
          (ensure-finalized class)
          (collect (prog1 (find-slot class (slot-definition-name it))
                     (assert it))))))

(defun effective-slots-for-slot-name (slot-name)
  (iter (for (class-name class) in-hashtable *persistent-classes*)
        (for slot = (find-slot class slot-name))
        (when slot (collect slot))))

(defun make-oid-columns ()
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

(defun make-columns-for-reference-slot (class-name column-name)
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

(defun make-bound-column (name)
  (make-instance 'column
                 :name (rdbms-name-for (concatenate-symbol name "-bound") :column)
                 :type (sql-boolean-type)
                 :constraints (list (sql-not-null-constraint))
                 :default-value #f))

(defun bound-column-of (slot)
  (bind ((column (first (columns-of slot))))
    (assert (ends-with (string-upcase (symbol-name (cl-rdbms::name-of column))) "BOUND"))
    (assert (typep (cl-rdbms::type-of column) 'sql-boolean-type))
    column))
