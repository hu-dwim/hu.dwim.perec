;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;;;;;;;;;;;;
;;; Load and cache

(defun instance-exists-in-database-p (instance)
  "Returns true if the instance can be found in the database"
  (and (oid-of instance)
       (not (null (select-records '(1)
                                  (list (name-of (primary-table-of (class-of instance))))
                                  (id-column-matcher-where-clause instance))))))

(defun debug-persistent-p (instance)
  "Same as persistent-p except it never prefetches slot values. Use for debug purposes."
  (if (slot-boundp instance 'persistent)
      (persistent-p instance)
      (progn
        ;; do not count this existence check as a select, because it will not execute in release code
        (when (oid-of instance)
          (decf (select-counter-of (command-counter-of *transaction*))))
        (setf (persistent-p instance) (instance-exists-in-database-p instance)))))

(defgeneric initialize-revived-instance (instance &key &allow-other-keys)
  (:documentation "When a revived instance is initialized slots marked with initialize-revived-slot-p will be passed down to be initialized by shared-initialize.")

  (:method ((instance persistent-object) &rest args &key oid &allow-other-keys)
           ;; TODO: use definer
           #-debug
           (declare (optimize (speed 3) (debug 0))
                    (dynamic-extent args))
           (assert oid)
           (bind ((slot-names
                   (iter (for slot in (class-slots (class-of instance)))
                         (if (typep slot 'persistent-effective-slot-definition)
                             (invalidate-cached-slot instance slot)
                             (collect (slot-definition-name slot))))))
             (apply #'shared-initialize instance slot-names args))))

(defgeneric make-revived-instance (class &key &allow-other-keys)
  (:documentation "Creates a new instance representing the given oid as its identity. The instance will not be associated with the current transaction nor will it be stored in the database. The instance may or may not be known to be either persistent or transient. This generic function should not be called outside of cl-perec but methods may be defined on it.")

  (:method ((class persistent-class) &rest args &key &allow-other-keys)
           (apply #'initialize-revived-instance (allocate-instance class) args)))

(defgeneric cache-instance (thing)
  (:documentation "Attaches an instance to the current transaction. The instance must be already present in the database, so load-instance would return an instance for it. The purpose of this method is to cache instances returned by a query or when the existence may be guaranteed by some other means.")

  (:method ((rdbms-values sequence))
           (cache-instance (rdbms-values->oid rdbms-values)))

  (:method ((oid oid))
           (aif (cached-instance-of oid)
                (prog1 it
                  (setf (persistent-p it) #t))
                (setf (cached-instance-of oid) (make-revived-instance (find-class (oid-class-name oid)) :oid oid :persistent #t))))

  (:method ((instance persistent-object))
           (debug-only (assert (debug-persistent-p instance)))
           (setf (cached-instance-of (oid-of instance)) instance)))

(define-condition instance-not-found-error (error)
  ((oid :accessor oid-of :initarg :oid))
  (:report (lambda (c stream)
             (format stream "Instance not found for oid ~A" (oid-of c)))))

(defgeneric load-instance (thing &key otherwise prefetch skip-existence-check)
  (:documentation "Loads an instance with the given oid and attaches it with the current transaction if not yet attached. If no such instance exists in the database then one of two things may happen. If the value of otherwise is a lambda function with one parameter then it is called with the given instance. Otherwise the value of otherwise is returned. If prefetch is false then only the identity of the instance is loaded, otherwise all slots are loaded. Note that the instance may not yet be committed into the database and therefore may not be seen by other transactions. Also instances not yet committed by other transactions are not returned according to transaction isolation rules. The instance returned will be kept for the duration of the transaction and any subsequent calls to load, select, etc. will return the exact same instance for which eq is required to return #t.")

  (:method ((instance persistent-object) &rest args)
           (apply #'load-instance (oid-of instance) args))

  (:method ((oid oid) &key (otherwise nil otherwise-provided-p) (prefetch #f) (skip-existence-check #f))
           (declare (ignore prefetch))
           (flet ((instance-not-found ()
                    (cond ((not otherwise-provided-p)
                           (error 'instance-not-found-error :oid oid))
                          ((functionp otherwise)
                           (funcall otherwise oid))
                          (t otherwise))))
             (aif (cached-instance-of oid)
                  it
                  (let ((new-instance (make-revived-instance (find-class (oid-class-name oid)) :oid oid)))
                    ;; REVIEW: is this the correct thing to do?
                    ;; we push the new-instance into the cache first
                    ;; even tough we are unsure if the instance is persistent or not
                    ;; because prefetching slots may recursively call load-instance from persistent-p
                    ;; we also want to have non persistent instances in the cache anyway
                    (setf (cached-instance-of (oid-of new-instance)) new-instance)
                    (if (or skip-existence-check (persistent-p new-instance))
                        new-instance
                        (instance-not-found)))))))

;;;;;;;;;
;;; Purge

(defgeneric purge-instance (instance)
  (:documentation "Purges the given instance without respect to associations and references to it.")
  
  (:method ((instance persistent-object))
           (ensure-exported (class-of instance))
           (dolist (table (data-tables-of (class-of instance)))
             (delete-records (name-of table)
                             (id-column-matcher-where-clause instance)))
           (update-instance-cache-for-deleted-instance instance)))

;; TODO: what about invalidating cache instances, references?
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
                                   (sql-in (sql-identifier :name +oid-id-column-name+)
                                           (sql-subquery :query
                                                         (apply #'sql-union
                                                                (mapcar #L(sql-select :columns (list +oid-id-column-name+)
                                                                                      :tables (list (name-of !1)))
                                                                        (cdr (primary-tables-of class)))))))))
               ;; delete instances from the primary tables of sub classes
               (dolist (table (list* class-primary-table sub-primary-tables))
                 (when table
                   (delete-records (name-of table))))))))

(defun purge-all-instances ()
  (purge-instances 'persistent-object))

;;;;;;;;
;;; Drop

(defun drop-persistent-classes ()
  (flet ((drop-table (owner)
           (when (table-exists-p (name-of owner))
             (rdbms::drop-table (name-of owner)))
           (invalidate-computed-slot owner 'ensure-exported)))
    (iter (for (class-name class) :in-hashtable *persistent-classes*)
          (awhen (primary-table-of class)
            (drop-table it)))
    (iter (for (association-name association) :in-hashtable *persistent-associations*)
          (awhen (primary-table-of association)
            (drop-table it)))))

;;;;;;;;
;;; Lock

(defgeneric lock-instance (instance &key wait)
  (:documentation "Lock instance in the current transaction. If wait is false and the instance cannot be locked then an condition will be thrown.")

  (:method :around (instance &key wait)
           (if wait
               (call-next-method)
               (handler-case
                   (progn
                     (call-next-method)
                     #t)
                 (error (e)
                        (declare (ignore e))
                        (return-from lock-instance #f)))))

  (:method ((instance persistent-object) &key (wait #t))
           (bind ((class (class-of instance))
                  (tables (data-tables-of class))
                  ((values table-aliases where-clause) (table-aliases-and-where-clause-for-instance instance tables)))
             (execute (sql-select :columns (list (make-instance 'sql-all-columns))
                                  :tables table-aliases
                                  :where where-clause
                                  :for :update
                                  :wait wait)))))

(defgeneric lock-slot (instance slot)
  ;; TODO: implement
  )

;;;;;;;;;;;;;;;;;;;;;
;;; Revive and reload

(defmacro revive-instance (place &rest args)
  "Load instance found in PLACE into the current transaction, update PLACE if needed. The instance being revived cannot be part of another ongoing transaction. Use load-instance if that is needed."
  (with-unique-names (instance)
    `(bind ((,instance ,place))
      (when ,instance
        (assert (or (not (instance-in-transaction-p ,instance))
                    (eq (transaction-of ,instance)
                        *transaction*))
                nil "The place ~S being revived holds an alive instance in another transaction. Maybe you should use load-instance and friends." ',place)
        (setf ,place (load-instance ,instance ,@args))))))

(defmacro revive-instances (&rest places)
  `(progn
    ,@(iter (for place :in places)
            (collect `(revive-instance ,place)))
    (values)))

(defmacro with-revived-instances (instances &body body)
  "Rebind the variables specified in INSTANCES, revive them in the current transaction and execute BODY in this lexical environment."
  (unless (every #'symbolp instances)
    (error "with-revived-instances works only on variables"))
  `(rebind (,@instances)
    ,@(iter (for instance :in instances)
            (collect `(revive-instance ,instance)))
    ,@body))

(defmacro with-revived-instance (instance &body body)
  "See WITH-REVIVED-INSTANCES."
  `(with-revived-instances (,instance)
    ,@body))

(defmacro with-reloaded-instances (instances &body body)
  "Rebind the variables specified in INSTANCES, reload them in the current transaction and execute BODY in this lexical environment."
  (unless (every #'symbolp instances)
    (error "with-reloaded-instances works only on variables"))
  `(bind ,(iter (for instance :in instances)
                (collect `(,instance (load-instance ,instance))))
    ,@body))

(defmacro with-reloaded-instance (instance &body body)
  "See WITH-RELOADED-INSTANCES."
  `(with-reloaded-instances (,instance)
    ,@body))

(defun singleton-variable-name-for (name)
  (bind ((name-string (symbol-name name)))
    (concatenate-symbol (subseq name-string 0 (1- (length name-string)))
                        "-singleton*"
                        (symbol-package name))))

(defmacro def-singleton-persistent-instance (name &body forms)
  (bind ((singleton-variable-name (singleton-variable-name-for name)))
    `(progn
     (defparameter ,singleton-variable-name nil)
     (define-symbol-macro ,name
         (progn
           (aif ,singleton-variable-name
                (load-instance it)
                (progn
                  (register-transaction-hook :before :rollback
                                             (lambda ()
                                               (setf ,singleton-variable-name nil)))
                  (setf ,singleton-variable-name
                        (progn
                          ,@forms)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Making instances persistent and transient

(defmethod make-persistent ((instance persistent-object))
  (ensure-oid instance)
  (update-instance-cache-for-created-instance instance)
  (store-all-slots instance)
  (setf (persistent-p instance) #t)
  (setf (cached-instance-of (oid-of instance)) instance)
  (bind ((class (class-of instance))
         (slots (persistent-effective-slots-of class)))
    (iter (for slot :in slots)
          (when (set-type-p (slot-definition-type slot))
            (invalidate-cached-slot instance slot)))))

(defmethod make-transient ((instance persistent-object))
  (with-caching-slot-values
    (bind ((class (class-of instance))
           ((values restored-slot-values restored-slots) (restore-all-slots instance)))
      (iter (for restored-slot-value in restored-slot-values)
            (for restored-slot in restored-slots)
            (setf (underlying-slot-boundp-or-value-using-class class instance restored-slot) restored-slot-value))))
  (remove-cached-instance instance)
  (setf (persistent-p instance) #f)
  (purge-instance instance))
