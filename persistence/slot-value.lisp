(in-package :cl-perec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Caching slot values in objects

(defparameter *cache-slot-values* #t
  "True means slot values will be cached in the slots of the persistent objects. Writing a slot still goes directly to the database but it will be also stored in the object. If the object's state is modified in the database it is up to the modifier to clear the list of cached slots from the object.")

(defparameter *bypass-database-access* #f
  "True means slot-value-using-class and friends will bypass database access and directly use the underlying CLOS object as a cache. It can be used for both reading and writing.")

(defparameter *propagate-cache-changes* #t
  "True means setting the slot of an object in the cache will propagate changes to other objects in the cache according to association end slot integrity rules.")

(defmacro with-caching-slot-values (&body body)
  `(bind ((*cache-slot-values* #t))
    ,@body))

(defmacro without-caching-slot-values (&body body)
  `(bind ((*cache-slot-values* #f))
    ,@body))

(defmacro with-bypassing-database-access (&body body)
  `(bind ((*bypass-database-access* #t))
    ,@body))

(defmacro without-bypassing-database-access (&body body)
  `(bind ((*bypass-database-access* #f))
    ,@body))

(defgeneric invalidate-all-cached-slots (object)
  (:documentation "Invalidates all cached slot values from the object.")

  (:method ((object persistent-object))
           (setf (cached-slots-of object) nil)
           (bind ((class (class-of object)))
             (iter (for slot in (persistent-slots-of class))
                   (setf (cached-slot-value-using-class class object slot) nil)))))

(defgeneric invalidate-cached-slot (object slot)
  (:documentation "Invalidates the cached slot value from the object.")

  (:method ((object persistent-object)
            (slot-name symbol))
           (invalidate-cached-slot object (find-slot (class-of object) slot-name)))

  (:method ((object persistent-object)
            (slot persistent-effective-slot-definition))
           (setf (cached-slot-value-using-class (class-of object) object slot) nil)
           (delete! slot (cached-slots-of object))))

(defgeneric propagate-cache-changes (class object slot new-value)
  (:documentation "Partially invalidate or update the cache to reflect setting the slot of object to new-value.")

  (:method ((class persistent-class)
            (object persistent-object)
            (slot persistent-effective-slot-definition) new-value)
           (debug-only (assert (debug-persistent-p object)))
           (values)))

(defgeneric slot-value-cached-p (object slot)
  (:documentation "Specifies if the given slot is cached in the ")
  
  (:method ((object persistent-object)
            (slot persistent-effective-slot-definition))
           (debug-only (assert (debug-persistent-p object)))
           (member slot (cached-slots-of object))))

(defgeneric cached-slot-value (object slot-name)
  (:documentation "Returns the cached value of the object's slot similar to slot-value.")
  
  (:method ((object persistent-object)
            (slot-name symbol))
           (debug-only (assert (debug-persistent-p object)))
           (with-bypassing-database-access
               (slot-value object slot-name))))

(defgeneric (setf cached-slot-value) (new-value object slot-name)
  (:documentation "Sets the cached value of the object's slot similar to (setf slot-value).")

  (:method (new-value
            (object persistent-object)
            (slot-name symbol))
           (debug-only (assert (debug-persistent-p object)))
           (with-bypassing-database-access
               (setf (slot-value object slot-name) new-value))))

(defgeneric cached-slot-value-using-class (class object slot)
  (:documentation "Returns the cached value of the object's slot similar to slot-value-using-class.")

  (:method ((class persistent-class)
            (object persistent-object)
            (slot persistent-effective-slot-definition))
           (debug-only (assert (debug-persistent-p object)))
           (with-bypassing-database-access
               (slot-value-using-class class object slot))))

(defgeneric (setf cached-slot-value-using-class) (new-value class object slot)
  (:documentation "Sets the cached value of the object's slot similar to (setf slot-value-using-class).")

  (:method (new-value
            (class persistent-class)
            (object persistent-object)
            (slot persistent-effective-slot-definition))
           (debug-only (assert (debug-persistent-p object)))
           (with-bypassing-database-access
               (setf (slot-value-using-class class object slot) new-value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLOS MOP slot-value-using-class and friends

(defmethod slot-value-using-class ((class persistent-class)
                                   (object persistent-object)
                                   (slot standard-effective-slot-definition))
  "Prefetches slot values when accessing the persistent slot of the object."
  (if (and (eq (slot-definition-name slot) 'persistent)
           (not (slot-boundp-using-class class object slot)))
      (if (prefetched-slots-of class)
          (bind (((values restored-slot-values restored-slots) (restore-prefetched-slots object #t)))
            (prog1 (setf (slot-value-using-class class object slot) (not (null restored-slots)))
              (iter (for restored-slot-value in restored-slot-values)
                    (for restored-slot in restored-slots)
                    (when (and *cache-slot-values*
                               (cached-slot-p restored-slot))
                      (setf (cached-slot-value-using-class class object restored-slot) restored-slot-value)))))
          (object-exists-in-database-p object))
      (call-next-method)))

(defmethod slot-value-using-class ((class persistent-class)
                                   (object persistent-object)
                                   (slot persistent-effective-slot-definition))
  "Reads the slot value from the database or the cache."
  (debug-only
    (assert (eq class (class-of object)))
    (assert (or *bypass-database-access*
                (not (debug-persistent-p object))
                (object-in-current-transaction-p object))))
  (if (or (not (persistent-p object))
          *bypass-database-access*
          (and *cache-slot-values*
               (slot-value-cached-p object slot)))
      ;; read the slot value from the cache
      (call-next-method)
      ;; restore the slot value from the database and put it in the underlying slot when appropriate
      (if (and *cache-slot-values*
               (prefetched-slot-p slot))
          ;; restore all prefetched slot values at once
          (bind (((values restored-slot-values restored-slots) (restore-prefetched-slots object))
                 (slot-value))
            (iter (for restored-slot-value in restored-slot-values)
                  (for restored-slot in restored-slots)
                  (when (eq slot restored-slot)
                    (setf slot-value restored-slot-value))
                  (when (cached-slot-p restored-slot)
                    (setf (cached-slot-value-using-class class object restored-slot) restored-slot-value)))
            slot-value)
          ;; only restore the requested slot value
          (bind (((values restored-slot-value restored-slot) (restore-slot object slot)))
            (when (and *cache-slot-values*
                       (cached-slot-p restored-slot))
              (setf (cached-slot-value-using-class class object restored-slot) restored-slot-value))
            restored-slot-value))))

(defmethod (setf slot-value-using-class) (new-value
                                          (class persistent-class)
                                          (object persistent-object)
                                          (slot persistent-effective-slot-definition))
  "Writes the new slot value to the database and the cache."
  (debug-only
    (assert (eq class (class-of object)))
    (assert (or *bypass-database-access*
                (not (debug-persistent-p object))
                (object-in-current-transaction-p object))))
  (bind ((persistent (persistent-p object)))
    ;; store slot value in the database
    (when (and (not *bypass-database-access*)
               persistent)
      (store-slot object slot new-value)
      (unless (modified-p object)
        (setf (modified-p object) #t)
        (insert-item (current-modified-objects) object)))
    ;; update slot value cache if appropriate
    (when (and persistent
               *propagate-cache-changes*)
      (bind ((*propagate-cache-changes* #f))
        (propagate-cache-changes class object slot new-value)))
    (when (and *cache-slot-values*
               (cached-slot-p slot)
               persistent)
      (pushnew slot (cached-slots-of object)))
    ;; store slot value in the underlying slot if appropriate
    (when (or (not persistent)
              (and *cache-slot-values*
                   (cached-slot-p slot))
              *bypass-database-access*)
      (call-next-method))
    new-value))

(defmethod slot-boundp-using-class ((class persistent-class)
                                    (object persistent-object)
                                    (slot persistent-effective-slot-definition))
  "Slots are always bound, they may or may not be nil though."
  ;; TODO: if we ever want to distinguish between nils and unbound slots we will have to extend the RDBMS mapping for those slots and introduce an extra column
  (debug-only
    (assert (eq class (class-of object)))
    (assert (or *bypass-database-access*
                (not (debug-persistent-p object))
                (object-in-current-transaction-p object))))
  (or (persistent-p object)
      (call-next-method)))

(defmethod update-instance-for-different-class :after ((previous-object persistent-object)
                                                       (current-object persistent-object)
                                                       &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  ;; TODO: update foreign key references according to class name
  (bind ((previous-class (class-of previous-object))
         (current-class (class-of current-object))
         (at-current-object (oid-matcher-writer-where-clause current-object nil)))
    (setf (class-name-of current-object) (name-of current-class))
    (dolist (table (data-tables-of current-class))
      (if (member table (data-tables-of previous-class))
          (update-records (name-of table)
                          (list (sql-column-of (class-name-column-of table)))
                          (list (class-name-value current-object))
                          at-current-object)
          ;; TODO: handle initargs
          (insert-records (name-of table)
                          (mapcar #'sql-column-of (oid-columns-of table))
                          (oid-values current-object))))
    (dolist (table (data-tables-of previous-class))
      (unless (member table (data-tables-of current-class))
        (delete-records (name-of table)
                        at-current-object)))))

;; TODO: use alexandria support for #+#. when finally available
;; TODO: finish it up, what aboud make-unbound, how to display boundness, etc...
#+#.(cl:when (cl:find-package "SWANK") '(:and))
(progn
  (defmethod swank::inspect-slot-for-emacs ((class persistent-class)
                                            (object persistent-object)
                                            (slot persistent-effective-slot-definition))
    (if (debug-persistent-p object)
        `(,@(if (slot-value-cached-p object slot)
                `("Cached, value is " (:value ,(standard-instance-access object (slot-definition-location slot)))
                  " "
                  (:action "[invalidate cache]" ,(lambda () (invalidate-cached-slot object slot))))
                `("Not cached"
                  " "
                  (:action "[read in]" ,(lambda () (slot-value-using-class class object slot)))))
          " "
          (:action "[make unbound]" ,(lambda () (slot-makunbound-using-class class object slot))))
        (call-next-method)))

  (defmethod swank::inspect-for-emacs ((object persistent-object) inspector)
    (bind (((values nil body) (call-next-method)))
      (values (if (debug-persistent-p object) "A persistent object." "A transient object.")
              (append `("Transaction: " (:value ,(when (object-in-transaction-p object) (transaction-of object))) (:newline))
                      body)))))
