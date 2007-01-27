;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Persistent class and slot meta objects

(defcclass* persistent-class (standard-class exportable)
  ((abstract
    (compute-as #f)
    :type boolean
    :documentation "An abstract persistent class cannot be instantiated but still can be used in associations and may have slots. Calling make-instance on an abstract persistent class will signal an error. On the other hand abstract classes might not have a primary table and thus handling the instances may require simpler or less SQL statements.")
   (persistent-direct-slots
    (compute-as (collect-if #L(typep !1 'persistent-direct-slot-definition) (class-direct-slots -self-)))
    :type (list persistent-direct-slot-definition)
    :documentation "The list of direct slots which are defined to be persistent in this class.")
   (persistent-effective-slots
    (compute-as (collect-if #L(typep !1 'persistent-effective-slot-definition) (class-slots -self-)))
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
    (compute-as (collect-if #'prefetched-p (persistent-effective-slots-of -self-)))
    :type (list persistent-effective-slot-definition)
    :documentation "The list of effective slots which will be loaded from and stored to the database at once when loading an instance of this class. Moreover when a persistent object is revived its prefetched slots will be loaded.")
   (non-prefetched-slots
    (compute-as (remove-if #'prefetched-p (persistent-effective-slots-of -self-)))
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
  (:documentation "Persistent class is a class meta object for classes. Standard defclass forms may be used to define persistent classes. A persistent class will have persistent slots unless marked with :persistent #f. A persistent slot should have type specification to be efficient both in storage and speed. The special type unbound must be used to mark slots which might be unbound."))

(defclass persistent-slot-definition-class (computed-class)
  ()
  (:documentation "This class serves a very special purpose, namely being able to return the very same instance in make-instance for slot definition meta objects."))

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
  (:documentation "Base class for both persistent direct and effective slot definitions."))

(defcclass* persistent-direct-slot-definition
    (persistent-slot-definition standard-direct-slot-definition)
  ((table
    :type (or null table)
    :documentation "A table where this direct slot has its columns if any. If the slot does not have a type specification then this is nil.")
   (columns
    :type (or null list)
    :documentation "A list of RDBMS columns created for this direct slot or nil. If the slot does not have a type specification then this is nil."))
  (:metaclass persistent-slot-definition-class)
  (:documentation "Class for persistent direct slot definitions."))

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
   (id-column
    (compute-as (bind ((type (remove-null-and-unbound-if-or-type (slot-definition-type -self-))))
                  (if (or (persistent-class-type-p type)
                          (set-type-p type))
                      (first (columns-of -self-)))))
    :type sql-column
    :documentation "This is filled for slots having set type.")
   (reader
    (compute-as (compute-reader (slot-definition-type -self-)))
    :type (or null function)
    :documentation "A one parameter function which transforms RDBMS data received as a list to the corresponding lisp object. This is present only for data table slots.")
   (writer
    (compute-as (compute-writer (slot-definition-type -self-)))
    :type (or null function)
    :documentation "A one parameter function which transforms a lisp object to the corresponding RDBMS data. This is present only for data table slots.")
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
  (:metaclass persistent-slot-definition-class)
  (:documentation "Class for persistent effective slot definitions."))

(defcclass* class-primary-table (table)
  ((oid-columns
    (compute-as (list (id-column-of -self-) (class-name-column-of -self-)))
    :type (list sql-column)
    :documentation "The list of RDBMS columns corresponding to the oid of this table.")
   (id-column
    (compute-as (find +id-column-name+ (columns-of -self-) :key 'cl-rdbms::name-of))
    :type sql-column
    :documentation "The RDBMS column of corresponding oid slot.")
   (class-name-column
    (compute-as (find +class-name-column-name+ (columns-of -self-) :key 'cl-rdbms::name-of))
    :type sql-column
    :documentation "The RDBMS column of corresponding oid slot."))
  (:documentation "This is a special table related to a persistent class."))

(defmethod describe-object ((object persistent-class) stream)
  (call-next-method)
  (aif (primary-table-of object)
       (progn
         (princ "The primary table is the following: ")
         (describe-object (primary-table-of object) stream))
       (princ (format nil "The primary tables are: ~A" (primary-tables-of object)) stream)))

;;;;;;;;;;
;;; Export

(defmethod export-to-rdbms ((class persistent-class))
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (mapc #'ensure-exported
        (collect-if #L(typep !1 'persistent-class)
                    (cdr (class-precedence-list class))))
  (mapc #'ensure-exported
        (collect-if #L(typep !1 'persistent-association)
                    (depends-on-of class)))
  (awhen (primary-table-of class)
    (ensure-exported it)))

;;;;;;;;;;;;
;;; Computed

(defgeneric compute-column-type (type &optional type-specification)
  (:documentation "Returns the RDBMS type for the given type.")

  (:method (type &optional type-specification)
           (declare (ignore type-specification))
           (error "Cannot map type ~A to RDBMS type" type))

  (:method ((type list) &optional type-specification)
           (declare (ignore type-specification))
           (compute-column-type (first type) type)))

(defgeneric compute-reader (type &optional type-specification)
  (:documentation "Maps a type to a one parameter lambda which will be called with the received RDBMS values.")

  (:method (type &optional type-specification)
           (declare (ignore type-specification))
           (error "Cannot map type ~A to a reader" type))

  (:method ((type list) &optional type-specification)
           (declare (ignore type-specification))
           (compute-reader (first type) type))

  (:method ((type symbol) &optional type-specification)
           (declare (ignore type-specification))
           (if (subtypep type 'persistent-object)
               'object-reader
               (call-next-method)))

  (:method ((type (eql 'or)) &optional type-specification)
           (bind ((or-unbound-type-p (or-unbound-type-p type-specification))
                  (or-null-type-p (or-null-type-p type-specification))
                  (wrapper (cond ((and or-unbound-type-p
                                       or-null-type-p)
                                  'unbound-or-null-reader)
                                 (or-unbound-type-p
                                  'unbound-reader)
                                 (or-null-type-p
                                  'null-reader)
                                 (t
                                  'identity))))
             (funcall wrapper (compute-reader (remove-null-and-unbound-if-or-type type-specification))))))

(defgeneric compute-writer (type &optional type-specification)
  (:documentation "Maps a type to a one parameter lambda which will be called with the slot value.")

  (:method (type &optional type-specification)
           (declare (ignore type-specification))
           (error "Cannot map type ~A to a writer" type))

  (:method ((type list) &optional type-specification)
           (declare (ignore type-specification))
           (compute-writer (first type) type))

  (:method ((type symbol) &optional type-specification)
           (declare (ignore type-specification))
           (if (persistent-class-type-p type)
               'object-writer
               (call-next-method)))

  (:method ((type persistent-class) &optional type-specification)
           (declare (ignore type-specification))
           'object-writer)

  (:method ((type (eql 'or)) &optional type-specification)
           (bind ((type (remove-null-and-unbound-if-or-type type-specification))
                  (or-unbound-type-p (or-unbound-type-p type-specification))
                  (or-null-type-p (or-null-type-p type-specification))
                  (or-unbound-and-null-type-p (and or-unbound-type-p
                                                   or-null-type-p))
                  (wrapper (cond (or-unbound-and-null-type-p
                                  'unbound-or-null-writer)
                                 (or-unbound-type-p
                                  'unbound-writer)
                                 (or-null-type-p
                                  'null-writer)
                                 (t
                                  (lambda (function column-number)
                                    (declare (ignore column-number))
                                    function)))))
             (funcall wrapper (compute-writer type)
                      (+ (cond ((persistent-class-type-p type)
                                2)
                               ((primitive-type-p type)
                                1)
                               (t (error "Cannot map type ~A to a writer" type)))
                         (if or-unbound-and-null-type-p
                             1
                             0))))))

(defgeneric compute-primary-table (class current-table)
  (:method ((class persistent-class) current-table)
           (flet ((primary-table-columns-for-class (class)
                    (mappend #L(mappend #L(when (eq (primary-table-of class) (table-of !1))
                                            (columns-of !1))
                                        (persistent-direct-slots-of !1))
                             (list* class (collect-if #L(typep !1 'persistent-class) (depends-on-of class))))))
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
                              (collect-if #L(typep !1 'persistent-class)
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
           (some #'table-of (direct-slots-of slot))))

(defgeneric compute-columns (slot)
  (:method :around ((slot persistent-direct-slot-definition))
           (when (slot-definition-type slot)
             (when-bind table (table-of slot)
               (call-next-method))))

  (:method ((slot persistent-direct-slot-definition))
           (bind ((name (slot-definition-name slot))
                  (type (slot-definition-type slot))
                  (class (slot-definition-class slot)))
             (awhen (remove-null-and-unbound-if-or-type type)
               (cond ((set-type-p it)
                      (make-columns-for-reference-slot (class-name class)
                                                       (strcat name "-for-" (class-name class))))
                     ((persistent-class-type-p it)
                      (make-columns-for-reference-slot (class-name class) name))
                     ((primitive-type-p it)
                      (append
                       (when (and (or-null-type-p type)
                                  (or-unbound-type-p type))
                         #+nil
                         (list
                          (make-instance 'column
                                         :name (rdbms-name-for (concatenate-symbol name "-bound"))
                                         :type (make-instance 'sql-boolean-type))))
                       (list
                        (make-instance 'column
                                       :name (rdbms-name-for name)
                                       :type (compute-column-type it)))))
                     (t
                      (error "Unknown slot type in slot ~A" slot))))))

  (:method ((slot persistent-effective-slot-definition))
           (some #'columns-of (direct-slots-of slot))))

;;;;;;;;
;;; Type

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

(defun or-unbound-type-p (type)
  (subtypep 'unbound type))

(defun or-null-type-p (type)
  (subtypep 'null type))

;;;;;;;;;;;
;;; Utility

(defparameter *persistent-classes* (make-hash-table)
  "A mapping from persistent class names to persistent objects.")

(defun find-persistent-class (name)
  (gethash name *persistent-classes*))

(defun (setf find-persistent-class) (new-value name)
  (setf (gethash name *persistent-classes*) new-value))

(defun persistent-class-p (class)
  (typep class 'persistent-class))

(defun persistent-class-name-p (name)
  (and name
       (symbolp name)
       (persistent-class-p (find-class name #f))))

(defun persistent-slot-p (slot)
  (typep slot 'persistent-slot-definition))

(defun effective-persistent-super-classes-for (class &optional (include-self #f))
  (collect-if #L(and (or include-self
                         (not (eq class !1)))
                     (typep !1 'persistent-class))
              (class-precedence-list class)))

(defun effective-persistent-sub-classes-for (class &optional (include-self #f))
  (delete-duplicates
   (append
    (when include-self (list class))
    (iter (for sub-class in (class-direct-subclasses class))
          (appending (effective-persistent-sub-classes-for sub-class #t))))))

(defun slot-definition-class (slot)
  "Returns the class to which the given slot belongs."
  #+sbcl(slot-value slot 'sb-pcl::%class)
  #-sbcl(not-yet-implemented))

(defun make-oid-columns ()
  "Creates a list of RDBMS columns that will be used to store the oid data of the objects in this table."
  (list
   (make-instance 'column
                  :name +id-column-name+
                  :type +oid-id-sql-type+
                  :constraints (list (make-instance 'sql-not-null-constraint)
                                     (make-instance 'sql-primary-key-constraint)))
   (make-instance 'column
                  :name +class-name-column-name+
                  :type +oid-class-name-sql-type+)))

(defun make-columns-for-reference-slot (class-name column-name)
  (bind ((id-column-name (rdbms-name-for (concatenate-symbol column-name "-id")))
         (id-index-name (rdbms-name-for (concatenate-symbol column-name "-id-on-" class-name "-idx")))
         (class-name-column-name (rdbms-name-for (concatenate-symbol column-name "-class-name"))))
    (list
     (make-instance 'column
                    :name id-column-name
                    :type +oid-id-sql-type+
                    :index (make-instance 'sql-index :name id-index-name))
     (make-instance 'column
                    :name class-name-column-name
                    :type +oid-class-name-sql-type+))))

(defun slot-accessor-p (name)
  (and (symbolp name)
       (effective-slots-for-accessor name)))

;; TODO: fix mop to append direct slots readers and store it in effective slots
(defun effective-slots-for-accessor (name)
  (iter (for (class-name class) in-hashtable *persistent-classes*)
        (awhen (find name (persistent-direct-slots-of class)
                     :key #'slot-definition-readers
                     :test #'member)
          (unless (class-finalized-p class)
            (finalize-inheritance class))
          (collect (prog1 (find-slot class (slot-definition-name it))
                     (assert it))))))
