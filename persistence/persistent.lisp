;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(defun object-exists-in-database-p (object)
  "Returns true if the object can be found in the database"
  (and (oid-of object)
       (not (null (select-records '(1)
                                  (list (name-of (primary-table-of (class-of object))))
                                  (id-column-matcher-where-clause object))))))

(defun debug-persistent-p (object)
  "Same as persistent-p except it never prefetches slot values. Use for debug purposes."
  (if (slot-boundp object 'persistent)
      (persistent-p object)
      (progn
        ;; do not count this existence check as a select, because it will not execute in release code
        (when (oid-of object)
          (decf (select-counter-of (command-counter-of *transaction*))))
        (setf (persistent-p object) (object-exists-in-database-p object)))))

(defgeneric initialize-revived-slot-p (slot)
  (:documentation "When a persistent instance is revived the slots marked here will be initialized by shared-initialize. The default implementation will not initialize persistent slots.")

  (:method (slot)
           #t)

  (:method ((slot persistent-effective-slot-definition))
           #f))

(defgeneric initialize-revived-instance (instance &key &allow-other-keys)
  (:documentation "When a revived instance is initialized slots marked with initialize-revived-slot-p will be passed down to be initialized by shared-initialize.")

  (:method ((instance persistent-object) &rest args &key oid &allow-other-keys)
           (assert oid)
           (bind ((slot-names
                   (iter (for slot in (class-slots (class-of instance)))
                         (when (initialize-revived-slot-p slot)
                           (collect (slot-definition-name slot))))))
             (apply #'shared-initialize instance slot-names args))))

(defgeneric make-revived-instance (class &key &allow-other-keys)
  (:documentation "Creates a new instance representing the given oid as its identity. The instance will not be associated with the current transaction nor will it be stored in the database. The instance may or may not be known to be either persistent or transient. This generic function should not be called outside of cl-perec but methods may be defined on it.")

  (:method ((class persistent-class) &rest args &key &allow-other-keys)
           (apply #'initialize-revived-instance (allocate-instance class) args)))

(defgeneric cache-object (thing)
  (:documentation "Attaches an object to the current transaction. The object must be already present in the database, so load-instance would return an instance for it. The purpose of this method is to cache objects returned by a query or when the existence may be guaranteed by some other means.")

  (:method ((values list))
           (assert (= 2 (length values)))
           (cache-object (make-oid :id (first values) :class-name (symbol-from-canonical-name (second values)))))

  (:method ((oid oid))
           (aif (cached-object-of oid)
                (prog1 it
                  (setf (persistent-p it) #t))
                (setf (cached-object-of oid) (make-revived-instance (find-class (oid-class-name oid)) :oid oid :persistent #t))))

  (:method ((object persistent-object))
           (debug-only (assert (debug-persistent-p object)))
           (setf (cached-object-of (oid-of object)) object)))

(define-condition object-not-found-error (error)
  ((oid :accessor oid-of :initarg :oid))
  (:report (lambda (c stream)
             (format stream "Object not found for oid ~A" (oid-of c)))))

(defgeneric load-instance (thing &key otherwise prefetch skip-existence-check)
  (:documentation "Loads an object with the given oid and attaches it with the current transaction if not yet attached. If no such object exists in the database then one of two things may happen. If the value of otherwise is a lambda function with one parameter then it is called with the given object. Otherwise the value of otherwise is returned. If prefetch is false then only the identity of the object is loaded, otherwise all slots are loaded. Note that the object may not yet be committed into the database and therefore may not be seen by other transactions. Also objects not yet committed by other transactions are not returned according to transaction isolation rules. The object returned will be kept for the duration of the transaction and any subsequent calls to load, select, etc. will return the exact same object for which eq is required to return #t.")

  (:method ((object persistent-object) &rest args)
           (apply #'load-instance (oid-of object) args))

  (:method ((oid oid) &key (otherwise nil otherwise-provided-p) (prefetch #f) (skip-existence-check #f))
           (declare (ignore prefetch))
           (flet ((object-not-found ()
                    (cond ((not otherwise-provided-p)
                           (error 'object-not-found-error :oid oid))
                          ((functionp otherwise)
                           (funcall otherwise oid))
                          (t otherwise))))
             (aif (cached-object-of oid)
                  it
                  (let ((new-object (make-revived-instance (find-class (oid-class-name oid)) :oid oid)))
                    ;; REVIEW: is this the correct thing to do?
                    ;; we push the new-object into the cache first
                    ;; even tough we are unsure if the object is persistent or not
                    ;; because prefetching slots may recursively call load-instance from persistent-p
                    ;; we also want to have non persistent objects in the cache anyway
                    (setf (cached-object-of (oid-of new-object)) new-object)
                    (if (or skip-existence-check (persistent-p new-object))
                        new-object
                        (object-not-found)))))))

(defgeneric purge-instance (object)
  (:documentation "Purges the given instance without respect to associations and references to it.")
  
  (:method ((object persistent-object))
           (ensure-exported (class-of object))
           (dolist (table (data-tables-of (class-of object)))
             (delete-records (name-of table)
                             (id-column-matcher-where-clause object)))))

(defgeneric purge-instances (class)
  (:documentation "Purges all instances of the given class without respect to associations and references.")

  (:method ((class-name symbol))
           (purge-instances (find-class class-name)))

  (:method ((class persistent-class))
           (ensure-exported class)
           (bind ((class-primary-table (primary-table-of class))
                  (super-classes (persistent-effective-super-classes-of class))
                  (sub-classes (persistent-effective-sub-classes-of class))
                  (super-primary-tables (mapcar #'primary-table-of super-classes))
                  (sub-primary-tables (mapcar #'primary-table-of sub-classes)))
             (mapc #'ensure-exported super-classes)
             (mapc #'ensure-exported sub-classes)
             (when (primary-tables-of class)
               ;; delete instances from the primary tables of super classes and non primary data tables of sub classes 
               (dolist (table (delete-if #L(or (eq !1 class-primary-table)
                                               (member !1 sub-primary-tables))
                                         (delete-duplicates
                                          (append super-primary-tables
                                                  (mappend #'data-tables-of sub-classes)))))
                 (when table
                   (delete-records (name-of table)
                                   (sql-in (sql-identifier :name +id-column-name+)
                                           (sql-subquery :query
                                                         (apply #'sql-union
                                                                (mapcar #L(sql-select :columns (list +id-column-name+)
                                                                                      :tables (list (name-of !1)))
                                                                        (cdr (primary-tables-of class)))))))))
               ;; delete instances from the primary tables of sub classes
               (dolist (table (list* class-primary-table sub-primary-tables))
                 (when table
                   (delete-records (name-of table))))))))

(defmacro revive-instance (place &rest args)
  "Load object found in PLACE into the current transaction, update PLACE if needed."
  (with-unique-names (instance)
    `(bind ((,instance ,place))
      (when ,instance
        (assert (or (not (instance-in-transaction-p ,instance))
                    (eq (transaction-of ,instance)
                        *transaction*)))
        (setf ,place (load-instance ,instance ,@args))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Making objects persistent and transient

(defmethod make-persistent ((object persistent-object))
  (ensure-oid object)
  (let ((created-objects (current-created-objects))
        (deleted-objects (current-deleted-objects)))
    (if (find-item deleted-objects object)
        (delete-item deleted-objects object)
        (insert-item created-objects object)))
  (store-all-slots object)
  (setf (persistent-p object) #t)
  (setf (cached-object-of (oid-of object)) object))

(defmethod make-transient ((object persistent-object))
  (let ((created-objects (current-created-objects))
        (deleted-objects (current-deleted-objects)))
    (if (find-item created-objects object)
        (delete-item created-objects object)
        (insert-item deleted-objects object)))
  (with-caching-slot-values
    (restore-all-slots object))
  (purge-instance object)
  (setf (persistent-p object) #f)
  (remove-cached-object object))
