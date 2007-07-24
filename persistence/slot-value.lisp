(in-package :cl-perec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Caching slot values in instances

(defconstant +vm-specific-unbound-slot-value+ sb-pcl::+slot-unbound+)

(defparameter *cache-slot-values* #t
  "True means slot values will be cached in the slots of the persistent instances. Writing a slot still goes directly to the database but it will be also stored in the instance. If the instance's state is modified in the database it is up to the modifier to clear the list of cached slots from the instance using the invalidate functions. The purpose of the slot value cache is to increase performance and reduce the number of database interactions during a transaction.")

;; TODO: why not storing a predefined value (i.e an internal symbol) in the slot when it is not cached

(defparameter *propagate-cache-changes* #t
  "True means setting the slot of an instance in the cache will propagate changes to other instances in the cache according to the association end slot integrity rules.")

(defgeneric invalidate-all-cached-slots (instance)
  (:documentation "Invalidates all cached slot values in the instance.")

  (:method ((instance persistent-object))
           (bind ((class (class-of instance)))
             (iter (for slot in (persistent-effective-slots-of class))
                   (cached-slot-makunbound-using-class class instance slot)))
           (setf (cached-slots-of instance) nil)))

(defgeneric invalidate-cached-slot (instance slot)
  (:documentation "Invalidates the given cached slot value in the instance.")

  (:method ((instance persistent-object) (slot-name symbol))
           (invalidate-cached-slot instance (find-slot (class-of instance) slot-name)))

  (:method ((instance persistent-object) (slot persistent-effective-slot-definition))
           (cached-slot-makunbound-using-class (class-of instance) instance slot)
           (delete! slot (cached-slots-of instance))))

(defgeneric propagate-cache-changes (class instance slot new-value)
  (:documentation "Partially invalidate or update the cache to reflect setting the slot of instance to new-value.")

  (:method ((class persistent-class) (instance persistent-object) (slot persistent-effective-slot-definition) new-value)
           (debug-only (assert (debug-persistent-p instance)))
           (values)))

(defun slot-value-cached-p (instance slot)
  "Specifies whether the given slot is cached in the instance or not."
  (debug-only (assert (debug-persistent-p instance)))
  (member slot (cached-slots-of instance)))

(defun cached-slot-value (instance slot-name)
  "Similar to slot-value but never interacts with the database."
  (debug-only (assert (debug-persistent-p instance)))
  (bind ((class (class-of instance))
         (slot (find-slot class slot-name)))
    (if slot
        (cached-slot-value-using-class class instance slot)
        (values (slot-missing class instance slot-name 'slot-value)))))

(defun (setf cached-slot-value) (new-value instance slot-name)
  "Similar to (setf slot-value) but never interacts with the database."
  (debug-only (assert (debug-persistent-p instance)))
  (bind ((class (class-of instance))
         (slot (find-slot class slot-name)))
    (if slot
        (setf (cached-slot-value-using-class class instance slot) new-value)
        (progn
          (slot-missing class instance slot-name 'slot-value)
          new-value))))

(defun cached-slot-boundp-or-value (instance slot-name)
  "Similar to slot-value-boundp-or-value but never interacts with the database."
  (debug-only (assert (debug-persistent-p instance)))
  (bind ((class (class-of instance))
         (slot (find-slot class slot-name)))
    (if slot
        (cached-slot-boundp-or-value-using-class class instance slot)
        (values (slot-missing class instance slot-name 'slot-value)))))

(defun (setf cached-slot-boundp-or-value) (new-value instance slot-name)
  "Similar to (setf slot-value-boundp-or-value) but never interacts with the database."
  (debug-only (assert (debug-persistent-p instance)))
  (bind ((class (class-of instance))
         (slot (find-slot class slot-name)))
    (if slot
        (setf (cached-slot-boundp-or-value-using-class class instance slot) new-value)
        (progn
          (slot-missing class instance slot-name 'slot-value)
          new-value))))

(defun cached-slot-value-using-class (class instance slot)
  "Returns the cached value of the instance's slot similar to slot-value-using-class but never interacts with the database."
  (debug-only (assert (debug-persistent-p instance)))
  (bind ((value (cached-slot-boundp-or-value-using-class class instance slot)))
    (if (eq value +unbound-slot-value+)
        (values (slot-unbound class instance (slot-definition-name slot)))
        value)))

(defun (setf cached-slot-value-using-class) (new-value class instance slot)
  "Sets the cached value of the instance's slot similar to (setf slot-value-using-class) but never interacts with the database."
  (debug-only (assert (debug-persistent-p instance)))
  (setf (cached-slot-boundp-or-value-using-class class instance slot) new-value))

(defun cached-slot-makunbound-using-class (class instance slot)
  "Makes the cached instance's slot unbound similar to slot-makunbound-using-class but never interacts with the database."
  (debug-only (assert (debug-persistent-p instance)))
  (setf (cached-slot-boundp-or-value-using-class class instance slot) +unbound-slot-value+))

(defun cached-slot-boundp-using-class (class instance slot)
  "Returns the cached boundness of the instance's slot similar to slot-boundp-using-class but never interacts with the database."
  (debug-only (assert (debug-persistent-p instance)))
  (not (eq +unbound-slot-value+ (cached-slot-boundp-or-value-using-class class instance slot))))

(defun cached-slot-boundp-or-value-using-class (class instance slot)
  "Either returns the cached slot value or the unbound slot marker. This method does not interact with the database."
  (declare (ignore class))
  (debug-only (assert (debug-persistent-p instance)))
  (bind ((value (standard-instance-access instance (slot-definition-location slot))))
    (if (eq value +vm-specific-unbound-slot-value+)
        +unbound-slot-value+
        value)))

(defun (setf cached-slot-boundp-or-value-using-class) (new-value class instance slot)
  "Either sets the slot value to the given new value or makes the slot unbound if the new value is the unbound marker. This method does not interact with the database."
  (declare (ignore class))
  (debug-only (assert (debug-persistent-p instance)))
  (pushnew slot (cached-slots-of instance))
  (setf (standard-instance-access instance (slot-definition-location slot))
        (if (eq +unbound-slot-value+ new-value)
            +vm-specific-unbound-slot-value+
            new-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLOS MOP slot-value-using-class and friends

(defmacro assert-instance-access ()
  `(assert (or (not persistent) (instance-in-current-transaction-p instance)) nil
    "Accessing a persistent ~A while it is not attached to the current transaction." instance))

(defmethod slot-value-using-class ((class persistent-class)
                                   (instance persistent-object)
                                   (slot standard-effective-slot-definition))
  "Prefetches persistent slot values when determining whether the instance is persistent or not."
  (debug-only
    (assert (eq class (class-of instance)))
    (assert (eq class (slot-definition-class slot))))
  ;; check for the persistent flag slot
  (if (and (eq (slot-definition-name slot) 'persistent)
           (not (slot-boundp-using-class class instance slot)))
      ;; prefetch if possible otherwise simple existence check
      (if (prefetched-slots-of class)
          (bind (((values restored-slot-values restored-slots) (restore-prefetched-slots instance #t)))
            ;; the persistent flag must be stored prior to caching any slot value
            (prog1 (setf (slot-value-using-class class instance slot) (not (null restored-slots)))
              ;; cache prefetched slots
              (iter (for restored-slot-value in restored-slot-values)
                    (for restored-slot in restored-slots)
                    (when (and *cache-slot-values*
                               (cache-p restored-slot))
                      (setf (cached-slot-boundp-or-value-using-class class instance restored-slot) restored-slot-value)))))
          ;; simple existence test
          (setf (slot-value-using-class class instance slot) (instance-exists-in-database-p instance)))
      (call-next-method)))

(defun slot-boundp-or-value-using-class (class instance slot call-next-method return-with)
  (debug-only
    (assert (eq class (class-of instance)))
    (assert (eq class (slot-definition-class slot))))
  (bind ((persistent (persistent-p instance)))
    (assert-instance-access)
    (if (or (not persistent)
            (and *cache-slot-values*
                 (slot-value-cached-p instance slot)))
        ;; read the slot value from the cache
        (funcall call-next-method)
        ;; restore the slot value from the database and put it in the underlying slot when appropriate
        (if (and *cache-slot-values*
                 (prefetch-p slot))
            ;; restore all prefetched slot values at once
            (bind (((values restored-slot-values restored-slots) (restore-prefetched-slots instance))
                   (slot-value))
              (iter (for restored-slot-value in restored-slot-values)
                    (for restored-slot in restored-slots)
                    (when (eq slot restored-slot)
                      (setf slot-value restored-slot-value))
                    (when (cache-p restored-slot)
                      (setf (cached-slot-boundp-or-value-using-class class instance restored-slot) restored-slot-value)))
              (funcall return-with slot-value))
            ;; only restore the requested slot value
            (bind (((values restored-slot-value restored-slot) (restore-slot instance slot)))
              (when (and *cache-slot-values*
                         (cache-p restored-slot))
                (setf (cached-slot-boundp-or-value-using-class class instance restored-slot) restored-slot-value))
              (funcall return-with restored-slot-value))))))

(defun (setf slot-boundp-or-value-using-class) (new-value class instance slot call-next-method)
  (debug-only
    (assert (eq class (class-of instance)))
    (assert (eq class (slot-definition-class slot))))
  (bind ((persistent (persistent-p instance)))
    (assert-instance-access)
    ;; store slot value in the database
    (when persistent
      (store-slot instance slot new-value)
      (update-cache-for-modified-instance instance))
    ;; update slot value cache if appropriate
    (when (and persistent
               *propagate-cache-changes*)
      (bind ((*propagate-cache-changes* #f))
        (propagate-cache-changes class instance slot new-value)))
    (when (and *cache-slot-values*
               (cache-p slot)
               persistent)
      (pushnew slot (cached-slots-of instance)))
    ;; store slot value in the underlying slot if appropriate
    (when (or (not persistent)
              (and *cache-slot-values*
                   (cache-p slot)))
      (funcall call-next-method))
    new-value))

(defmethod slot-value-using-class ((class persistent-class)
                                   (instance persistent-object)
                                   (slot persistent-effective-slot-definition))
  "Reads the slot value from the database or the cache."
  (slot-boundp-or-value-using-class class instance slot #'call-next-method
                                    #L(if (eq !1 +unbound-slot-value+)
                                          (slot-unbound class instance (slot-definition-name slot))
                                          !1)))

(defmethod (setf slot-value-using-class) (new-value
                                          (class persistent-class)
                                          (instance persistent-object)
                                          (slot persistent-effective-slot-definition))
  "Writes the new slot value to the database and the cache."
  (setf (slot-boundp-or-value-using-class class instance slot #'call-next-method) new-value))

(defmethod slot-boundp-using-class ((class persistent-class)
                                    (instance persistent-object)
                                    (slot persistent-effective-slot-definition))
  "Reads boundness from the database or the cache."
  (slot-boundp-or-value-using-class class instance slot #'call-next-method #L(not (eq +unbound-slot-value+ !1))))

(defmethod slot-makunbound-using-class ((class persistent-class)
                                        (instance persistent-object)
                                        (slot persistent-effective-slot-definition))
  "Writes boundness to the database and the cache."
  (setf (slot-boundp-or-value-using-class class instance slot #'call-next-method) +unbound-slot-value+)
  instance)

;; TODO: add tests
(defmethod update-instance-for-different-class :after ((previous-instance persistent-object)
                                                       (current-instance persistent-object)
                                                       &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  ;; TODO: update foreign key references according to class name
  (bind ((previous-class (class-of previous-instance))
         (current-class (class-of current-instance))
         (current-oid (oid-of current-instance))
         (at-current-instance (id-column-matcher-where-clause current-instance)))
    (setf (oid-class-name current-oid) (name-of current-class))
    (dolist (table (data-tables-of current-class))
      (if (member table (data-tables-of previous-class))
          (update-records (name-of table)
                          (list (class-name-column-of table))
                          (list (oid-class-name current-oid))
                          at-current-instance)
          ;; TODO: handle initargs
          (insert-record (name-of table)
                         (oid-columns-of table)
                         (oid->rdbms-values current-oid))))
    (dolist (table (data-tables-of previous-class))
      (unless (member table (data-tables-of current-class))
        (delete-records (name-of table)
                        at-current-instance)))))

;;;;;;;;;;;;;;;;;;;;;
;;; Slime integration

#+#.(cl:when (cl:find-package "SWANK") '(:and))
(progn
  (defmethod swank::inspect-slot-for-emacs ((class persistent-class)
                                            (instance persistent-object)
                                            (slot persistent-effective-slot-definition))
    (if (debug-persistent-p instance)
        `(,@(if (slot-value-cached-p instance slot)
                `("Cached, value is " (:value ,(standard-instance-access instance (slot-definition-location slot)))
                  " "
                  (:action "[invalidate cache]" ,(lambda () (invalidate-cached-slot instance slot))))
                `("Not cached"
                  " "
                  (:action "[read in]" ,(lambda () (slot-value-using-class class instance slot)))))
          " "
          (:action "[make unbound]" ,(lambda () (slot-makunbound-using-class class instance slot))))
        (call-next-method)))

  (defmethod swank::inspect-for-emacs ((instance persistent-object) inspector)
    (bind ((result (multiple-value-list (call-next-method))))
      (if (= (length result) 1)
          (progn
            (setf result (first result))
            (bind ((content (getf result :content)))
              (setf (getf result :content)
                    (append `("Transaction: " (:value ,(when (instance-in-transaction-p instance) (transaction-of instance))) (:newline))
                            content))
              (setf (getf result :title)
                    (if (debug-persistent-p instance) "A persistent instance" "A transient instance"))
              result))
          ;; we do nothing with the old inspect protocol...
          (values-list result)))))
