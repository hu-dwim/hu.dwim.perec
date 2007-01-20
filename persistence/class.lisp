;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Persistent class and slot meta objects

(defclass* persistent-class (standard-class)
  ((persistent-slots
    (compute-as nil)
    :type (list persistent-effective-slot-definition))
   (primary-table
    (compute-as nil)
    :type table
    :documentation "The table which holds the oid and the data of the direct slots of this class. If the class is abstract and does not have any direct slots then it will not have a primary table. A primary table if exists also contains one and only one record per instance of its class.")
   (primary-tables
    (compute-as nil)
    :type (list class-primary-table)
    :documentation "The smallest set of tables which hold all instances and only the instances of this class by having one and only one record per instance. May contain functional nodes such as union, append.")
   (data-tables
    (compute-as nil)
    :type (list table)
    :documentation "All the tables which hold data of an instance of this class.")
   (prefetched-slots
    (compute-as nil)
    :type (list persistent-effective-slot-definition)
    :documentation "List of effective slots which will be loaded and stored at once when loading an instance of this class.")
   (non-prefetched-slots
    (compute-as nil)
    :type (list effective-slot)
    :documentation "List of effective slots which will be loaded and stored lazily and separately from other slots."))
  (:metaclass computed-class))

(defclass* persistent-slot-definition (standard-slot-definition)
  ((prefetched-slot
    (compute-as nil)
    :type boolean
    :documentation "Prefetched slots are loaded from and stored into the database at once. A prefetched slot must be in a table which can be accessed using a where clause matching to the id of the object thus it must be in a data table.")
   (cached-slot
    (compute-as nil)
    :type boolean
    :documentation "All prefetched slots are cached slots but the opposite may not be true. When a cached slot is loaded it's value will be stored in the CLOS object for fast subsequent read operations. Also whenever a cached slot is set the value will be remembered."))
  (:metaclass computed-class))

(defclass* accessor ()
  ((effective-slot
    :type persistent-effective-slot-definition
    :documentation "The effective slot meta object to which this accessor belongs.")
   (where-clause
    :type function
    :documentation "This function provides the SQL where clause to access the slot in the RDBMS.")
   (transformer
    :type function
    :documentation "A function which transforms between a lisp object and the corresponding RDBMS data. The direction of the transformation depends on whether the accessor is a reader or a writer.")))

(defclass* persistent-direct-slot-definition
    (persistent-slot-definition standard-direct-slot-definition)
  ()
  (:metaclass computed-class))

(defclass* persistent-effective-slot-definition
    (persistent-slot-definition standard-effective-slot-definition)
  ((table
    (compute-as nil)
    :type table
    :documentation "An RDBMS table object which will be queried or updated to get and set the data of this slot.")
   (columns
    (compute-as nil)
    :type (list column)
    :documentation "Several RDBMS columns may be mapped to a single effective slot.")
   (reader
    (compute-as nil)
    :type accessor
    :documentation "An accessor object which describes how to read this slot.")
   (writer
    (compute-as nil)
    :type accessor
    :documentation "An accessor object which describes how to write this slot."))
  (:metaclass computed-class))

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

(eval-always
  (pushnew :persistent *allowed-slot-definition-properties*))

;;;;;;;;;;;;;;;
;;; MOP methods 

;; allows persistent keyword argument for persistent-direct-slot-definitions according to CLOS mop
(defmethod shared-initialize :around ((slot persistent-direct-slot-definition) slot-names
                                      &rest args &key persistent &allow-other-keys)
  (declare (ignore persistent))
  (apply #'call-next-method slot slot-names args))

(defmethod initialize-instance :around ((class persistent-class)
                                        &rest args
                                        &key name direct-slots direct-superclasses
                                        &allow-other-keys)
  (let ((result (apply #'call-next-method
                       class
                       :direct-slots (process-direct-slot-definitions direct-slots)
                       :direct-superclasses (ensure-persistent-object-class name direct-superclasses)
                       #+lispworks :optimize-slot-access #+lispworks nil 
                       (remove-keywords args :direct-slots))))
    result))

(defmethod reinitialize-instance :around ((class persistent-class)
                                          &rest args
                                          &key direct-slots direct-superclasses
                                          &allow-other-keys)
  (let ((result (apply #'call-next-method
                       class
                       :direct-slots (process-direct-slot-definitions direct-slots)
                       :direct-superclasses (ensure-persistent-object-class (class-name class) direct-superclasses)
                       #+lispworks :optimize-slot-access #+lispworks nil
                       (remove-keywords args :direct-slots))))
    result))

(defmethod validate-superclass ((class standard-class)
                                (superclass persistent-class))
  t)

(defmethod validate-superclass ((class persistent-class)
                                (superclass standard-class))
  t)

(defmethod direct-slot-definition-class ((class persistent-class)
                                         &key persistent &allow-other-keys)
  (if persistent
      (find-class 'persistent-direct-slot-definition)
      (call-next-method)))

(defmethod effective-slot-definition-class ((class persistent-class)
                                            &key &allow-other-keys)
  (declare (special %persistent%))
  (if %persistent%
      (find-class 'persistent-effective-slot-definition)
      (call-next-method)))

(defmethod compute-effective-slot-definition ((class persistent-class)
                                              slot-name
                                              direct-slot-definitions)
  ;; the special %persistent% will be passed down to effective-slot-definition-class
  ;; there's no standard way to call the default slot option merge algorithm, so we
  ;; cannot extend it with our own in a nice way
  ;; the persistent flag is t for an effective slot if any of its direct slots is persistent
  (let* ((%persistent% (some (lambda (slot)
                               (typep slot 'persistent-direct-slot-definition))
                             direct-slot-definitions))
         (effective-slot-definition (call-next-method)))
    (declare (special %persistent%))
    effective-slot-definition))

;;;;;;;;;;;;;;;;;;
;;; Helper methods

(defun ensure-persistent-object-class (name direct-superclasses)
  (unless (eq 'persistent-object name)
    (let ((persistent-object (find-class 'persistent-object))
          (persistent-class (find-class 'persistent-class)))
      (if (find-if (lambda (direct-superclass)
                     (member persistent-class
                             (compute-class-precedence-list
                              (class-of direct-superclass))))
                   direct-superclasses)
          direct-superclasses
          (cons persistent-object direct-superclasses)))))

(defun process-direct-slot-definitions (direct-slots)
  (loop for direct-slot :in direct-slots
        collect (if (getf direct-slot :persistent)
                    direct-slot
                    (if (get-properties direct-slot '(:persistent))
                        ;; remove :persistent nil
                        (remove-keywords direct-slot :persistent)
                        ;; add default :persistent t
                        (append direct-slot '(:persistent t))))))
