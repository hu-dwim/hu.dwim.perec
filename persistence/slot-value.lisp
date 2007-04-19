(in-package :cl-perec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Caching slot values in objects

(defparameter *cache-slot-values* #t
  "True means slot values will be cached in the slots of the persistent objects. Writing a slot still goes directly to the database but it will be also stored in the object. If the object's state is modified in the database it is up to the modifier to clear the list of cached slots from the object using the invalidate functions. The purpose of the slot value cache is to increases performance and reduce the number of database interactions during a transaction.")

;; TODO: shouldn't we use standard-instance-access instead of this special? (probably would be more efficient)
(defparameter *bypass-database-access* #f
  "True means slot-value-using-class and friends will bypass database access and directly use the underlying CLOS object as a cache. It can be used for reading, writing, making unbound and checking boundness of slots.")

(defparameter *propagate-cache-changes* #t
  "True means setting the slot of an object in the cache will propagate changes to other objects in the cache according to the association end slot integrity rules.")

(defgeneric invalidate-all-cached-slots (object)
  (:documentation "Invalidates all cached slot values in the object.")

  (:method ((object persistent-object))
           (setf (cached-slots-of object) nil)
           (bind ((class (class-of object)))
             (iter (for slot in (persistent-effective-slots-of class))
                   (cached-slot-makunbound-using-class class object slot)))))

(defgeneric invalidate-cached-slot (object slot)
  (:documentation "Invalidates the given cached slot value in the object.")

  (:method ((object persistent-object) (slot-name symbol))
           (invalidate-cached-slot object (find-slot (class-of object) slot-name)))

  (:method ((object persistent-object) (slot persistent-effective-slot-definition))
           (cached-slot-makunbound-using-class (class-of object) object slot)
           (delete! slot (cached-slots-of object))))

(defgeneric propagate-cache-changes (class object slot new-value)
  (:documentation "Partially invalidate or update the cache to reflect setting the slot of object to new-value.")

  (:method ((class persistent-class) (object persistent-object) (slot persistent-effective-slot-definition) new-value)
           (debug-only (assert (debug-persistent-p object)))
           (values)))

(defgeneric slot-value-cached-p (object slot)
  (:documentation "Specifies whether the given slot is cached in the object or not.")
  
  (:method ((object persistent-object) (slot persistent-effective-slot-definition))
           (debug-only (assert (debug-persistent-p object)))
           (member slot (cached-slots-of object))))

(defun cached-slot-value (object slot-name)
  "Similar to slot-value but never interacts with the database."
  (debug-only (assert (debug-persistent-p object)))
  (with-bypassing-database-access
    (slot-value object slot-name)))

(defun (setf cached-slot-value) (new-value object slot-name)
  "Similar to (setf slot-value) but never interacts with the database."
  (debug-only (assert (debug-persistent-p object)))
  (with-bypassing-database-access
    (setf (slot-value object slot-name) new-value)))

(defun cached-slot-boundp-or-value (object slot-name)
  "Similar to slot-value-boundp-or-value but never interacts with the database."
  (debug-only (assert (debug-persistent-p object)))
  (bind ((class (class-of object)))
    (cached-slot-boundp-or-value-using-class class object (find-slot class slot-name))))

(defun (setf cached-slot-boundp-or-value) (new-value object slot-name)
  "Similar to (setf slot-value-boundp-or-value) but never interacts with the database."
  (debug-only (assert (debug-persistent-p object)))
  (bind ((class (class-of object)))
    (setf (cached-slot-boundp-or-value-using-class class object (find-slot class slot-name))
          new-value)))

(defgeneric cached-slot-value-using-class (class object slot)
  (:documentation "Returns the cached value of the object's slot similar to slot-value-using-class but never interacts with the database.")

  (:method ((class persistent-class) (object persistent-object) (slot persistent-effective-slot-definition))
           (debug-only (assert (debug-persistent-p object)))
           (with-bypassing-database-access
             (slot-value-using-class class object slot))))

(defgeneric (setf cached-slot-value-using-class) (new-value class object slot)
  (:documentation "Sets the cached value of the object's slot similar to (setf slot-value-using-class) but never interacts with the database.")

  (:method (new-value (class persistent-class) (object persistent-object) (slot persistent-effective-slot-definition))
           (debug-only (assert (debug-persistent-p object)))
           (with-bypassing-database-access
             (setf (slot-value-using-class class object slot) new-value))))

(defgeneric cached-slot-makunbound-using-class (class object slot)
  (:documentation "Makes the cached object's slot unbound similar to slot-makunbound-using-class but never interacts with the database.")

  (:method ((class persistent-class) (object persistent-object) (slot persistent-effective-slot-definition))
           (debug-only (assert (debug-persistent-p object)))
           (with-bypassing-database-access
             (slot-makunbound-using-class class object slot))))

(defgeneric cached-slot-boundp-using-class (class object slot)
  (:documentation "Returns the cached boundness of the object's slot similar to slot-boundp-using-class but never interacts with the database.")

  (:method ((class persistent-class) (object persistent-object) (slot persistent-effective-slot-definition))
           (debug-only (assert (debug-persistent-p object)))
           (with-bypassing-database-access
             (slot-boundp-using-class class object slot))))

(defgeneric cached-slot-boundp-or-value-using-class (class object slot)
  (:documentation "Either returns the cached slot value or the unbound slot marker. This method does not interact with the database.")

  (:method ((class persistent-class) (object persistent-object) (slot persistent-effective-slot-definition))
           (with-bypassing-database-access
             (if (not (slot-boundp-using-class class object slot))
                 +unbound-slot-value+
                 (slot-value-using-class class object slot)))))

(defgeneric (setf cached-slot-boundp-or-value-using-class) (new-value class object slot)
  (:documentation "Either sets the slot value to the given new value or makes the slot unbound if the new value is the unbound marker. This method does not interact with the database.")

  (:method (new-value (class persistent-class) (object persistent-object) (slot persistent-effective-slot-definition))
           (debug-only (assert (debug-persistent-p object)))
           (with-bypassing-database-access
             (if (eq +unbound-slot-value+ new-value)
                 (slot-makunbound-using-class class object slot)
                 (setf (slot-value-using-class class object slot) new-value)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLOS MOP slot-value-using-class and friends

(defmethod slot-value-using-class ((class persistent-class)
                                   (object persistent-object)
                                   (slot standard-effective-slot-definition))
  "Prefetches persistent slot values when determining whether the object is persistent or not."
  (debug-only
    (assert (eq class (class-of object)))
    (assert (eq class (slot-definition-class slot))))
  ;; check for the persistent flag slot
  (if (and (not *bypass-database-access*)
           (eq (slot-definition-name slot) 'persistent)
           (not (slot-boundp-using-class class object slot)))
      ;; prefetch if possible otherwise simple existence check
      (if (prefetched-slots-of class)
          (bind (((values restored-slot-values restored-slots) (restore-prefetched-slots object #t)))
            ;; the persistent flag must be stored prior to caching any slot value
            (prog1 (setf (slot-value-using-class class object slot) (not (null restored-slots)))
              ;; cache prefetched slots
              (iter (for restored-slot-value in restored-slot-values)
                    (for restored-slot in restored-slots)
                    (when (and *cache-slot-values*
                               (cache-p restored-slot))
                      (setf (cached-slot-boundp-or-value-using-class class object restored-slot) restored-slot-value)))))
          ;; simple existence test
          (setf (slot-value-using-class class object slot) (object-exists-in-database-p object)))
      (call-next-method)))

(defun slot-boundp-or-value-using-class (class object slot call-next-method return-with)
  (debug-only
    (assert (eq class (class-of object)))
    (assert (eq class (slot-definition-class slot))))
  (bind ((persistent (persistent-p object)))
    (assert (or *bypass-database-access*
                (not persistent)
                (instance-in-current-transaction-p object)))
    (if (or (not persistent)
            *bypass-database-access*
            (and *cache-slot-values*
                 (slot-value-cached-p object slot)))
        ;; read the slot value from the cache
        (funcall call-next-method)
        ;; restore the slot value from the database and put it in the underlying slot when appropriate
        (if (and *cache-slot-values*
                 (prefetch-p slot))
            ;; restore all prefetched slot values at once
            (bind (((values restored-slot-values restored-slots) (restore-prefetched-slots object))
                   (slot-value))
              (iter (for restored-slot-value in restored-slot-values)
                    (for restored-slot in restored-slots)
                    (when (eq slot restored-slot)
                      (setf slot-value restored-slot-value))
                    (when (cache-p restored-slot)
                      (setf (cached-slot-boundp-or-value-using-class class object restored-slot) restored-slot-value)))
              (funcall return-with  slot-value))
            ;; only restore the requested slot value
            (bind (((values restored-slot-value restored-slot) (restore-slot object slot)))
              (when (and *cache-slot-values*
                         (cache-p restored-slot))
                (setf (cached-slot-boundp-or-value-using-class class object restored-slot) restored-slot-value))
              (funcall return-with restored-slot-value))))))

(defun (setf slot-boundp-or-value-using-class) (new-value class object slot call-next-method)
  (debug-only
    (assert (eq class (class-of object)))
    (assert (eq class (slot-definition-class slot))))
  (bind ((persistent (persistent-p object)))
    (assert (or *bypass-database-access*
                (not persistent)
                (instance-in-current-transaction-p object)))
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
               (cache-p slot)
               persistent)
      (pushnew slot (cached-slots-of object)))
    ;; store slot value in the underlying slot if appropriate
    (when (or (not persistent)
              (and *cache-slot-values*
                   (cache-p slot))
              *bypass-database-access*)
      (funcall call-next-method))
    new-value))

(defmethod slot-value-using-class ((class persistent-class)
                                   (object persistent-object)
                                   (slot persistent-effective-slot-definition))
  "Reads the slot value from the database or the cache."
  (slot-boundp-or-value-using-class class object slot #'call-next-method #'identity))

(defmethod (setf slot-value-using-class) (new-value
                                          (class persistent-class)
                                          (object persistent-object)
                                          (slot persistent-effective-slot-definition))
  "Writes the new slot value to the database and the cache."
  (setf (slot-boundp-or-value-using-class class object slot #'call-next-method) new-value))

(defmethod slot-boundp-using-class ((class persistent-class)
                                    (object persistent-object)
                                    (slot persistent-effective-slot-definition))
  "Reads boundness from the database or the cache."
  (slot-boundp-or-value-using-class class object slot #'call-next-method #L(not (eq +unbound-slot-value+ !1))))

(defmethod slot-makunbound-using-class ((class persistent-class)
                                        (object persistent-object)
                                        (slot persistent-effective-slot-definition))
  "Writes boundness to the database and the cache."
  (setf (slot-boundp-or-value-using-class class object slot #'call-next-method) +unbound-slot-value+)
  object)

(defmethod update-instance-for-different-class :after ((previous-object persistent-object)
                                                       (current-object persistent-object)
                                                       &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  ;; TODO: update foreign key references according to class name
  (bind ((previous-class (class-of previous-object))
         (current-class (class-of current-object))
         (at-current-object (id-column-matcher-where-clause current-object)))
    (setf (class-name-of current-object) (name-of current-class))
    (dolist (table (data-tables-of current-class))
      (if (member table (data-tables-of previous-class))
          (update-records (name-of table)
                          (list (class-name-column-of table))
                          (list (class-name-value current-object))
                          at-current-object)
          ;; TODO: handle initargs
          (insert-records (name-of table)
                          (oid-columns-of table)
                          (oid-values current-object))))
    (dolist (table (data-tables-of previous-class))
      (unless (member table (data-tables-of current-class))
        (delete-records (name-of table)
                        at-current-object)))))

;;;;;;;;;;;;;;;;;;;;;
;;; Slime integration

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
    (bind ((result (multiple-value-list (call-next-method))))
      (if (= (length result) 1)
          (progn
            (setf result (first result))
            (bind ((content (getf result :content)))
              (setf (getf result :content)
                    (append `("Transaction: " (:value ,(when (instance-in-transaction-p object) (transaction-of object))) (:newline))
                            content))
              (setf (getf result :title)
                    (if (debug-persistent-p object) "A persistent object" "A transient object"))
              result))
          ;; we do nothing with the old inspect protocol...
          (values-list result)))))
