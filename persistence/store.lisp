(in-package :cl-perec)

;;;;;;;;;;;;;
;;; Constants

(defparameter *lazy-collections* #f
  "True means slot-value-using-class will by default return lazy collections.")

(eval-always
  (unless (fboundp 'make-unbound-value)
    (defstruct unbound-value
      "This structure is used for the unbound slot value marker. The type for that marker must be a subtype of t and cannot be a subtype of any other type.")))

(define-constant +unbound-slot-value+ (make-unbound-value)
  :test equalp
  :documentation "This value is used to signal unbound slot value returned from database.")

(defmethod make-load-form ((instance unbound-value) &optional environment)
  (declare (ignore environment))
  '(make-unbound-value))

(defun unbound-slot-value-p (value)
  (eq +unbound-slot-value+ value))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RDBMS slot restorers

(defun restore-slot-value (slot rdbms-values index)
  "Provides convenient access to the arguments in the debugger."
  (declare (optimize (debug 3)))
  (funcall (reader-of slot) rdbms-values index))

(defun restore-slot-set (instance slot)
  "Restores the non lazy list without local side effects from the database."
  (map 'list #L(object-reader !1 0)
       (select-records (oid-columns-of (table-of slot))
                       (list (name-of (table-of slot)))
                       (id-column-matcher-where-clause instance (id-column-of slot))
                       (let ((type (slot-definition-type slot)))
                         (if (ordered-set-type-p type)
                             ;; TODO: use reflection instead of third
                             (list (sql-identifier :name (rdbms-name-for (third type)))))))))

(defun restore-1-n-association-end-set (instance slot)
  "Restores the non lazy list association end value without local side effects from the database."
  (restore-slot-set instance slot))

(defun restore-m-n-association-end-set (instance slot)
  "Restores the non lazy list association end value without local side effects from the database."
  (bind ((other-slot (other-association-end-of slot)))
    (map 'list #L(object-reader !1 0)
         (select-records (columns-of slot)
                         (list (name-of (table-of slot)))
                         (id-column-matcher-where-clause instance (id-column-of other-slot))))))

(defun restore-slot (instance slot)
  "Restores a single slot without local side effects from the database."
  (values
   (cond ((and (typep slot 'persistent-association-end-effective-slot-definition)
               (eq (association-kind-of (association-of slot)) :1-1)
               (secondary-association-end-p slot))
          (let ((records
                 (select-records +oid-column-names+
                                 (list (name-of (table-of slot)))
                                 (sql-= (id-of instance)
                                        (sql-identifier :name (id-column-of slot))))))
            (unless (zerop (length records))
              (restore-slot-value slot (elt-0 records) 0))))
         ((and (typep slot 'persistent-association-end-effective-slot-definition)
               (eq (association-kind-of (association-of slot)) :1-n)
               (eq (cardinality-kind-of slot) :n))
          (if *lazy-collections*
              (make-instance 'persistent-1-n-association-end-set-container :instance instance :slot slot)
              (restore-1-n-association-end-set instance slot)))
         ((and (typep slot 'persistent-association-end-effective-slot-definition)
               (eq (association-kind-of (association-of slot)) :m-n))
          (if *lazy-collections*
              (make-instance 'persistent-m-n-association-end-set-container :instance instance :slot slot)
              (restore-m-n-association-end-set instance slot)))
         ((set-type-p (normalized-type-of slot))
          (if *lazy-collections*
              (make-instance 'persistent-slot-set-container :instance instance :slot slot)
              (restore-slot-set instance slot)))
         (t
          (bind ((record
                  (elt-0
                   (select-records (columns-of slot)
                                   (list (name-of (table-of slot)))
                                   (id-column-matcher-where-clause instance)))))
            (restore-slot-value slot record 0))))
   slot))

(defun restore-prefetched-slots (instance &optional (allow-missing #f))
  "Restores all prefetched slots at once without local side effects from the database. Executes a single select statement."
  (if-bind slots (prefetched-slots-of (class-of instance))
    (bind ((tables (delete-duplicates (mapcar #'table-of slots)))
           ((values table-aliases where-clause) (table-aliases-and-where-clause-for-instance instance tables))
           (records
            (select-records (mapcan (lambda (slot)
                                      (mapcar (lambda (column)
                                                (sql-column-alias :table (name-of (table-of slot)) :column column))
                                              (columns-of slot)))
                                    slots)
                            table-aliases
                            where-clause))
           (record (unless (and allow-missing
                                (zerop (length records)))
                     (assert (= 1 (length records)))
                     (elt-0 records))))
      (when record
        (values
         (iter (for i :first 0 :then (+ i (length (columns-of slot))))
               (for slot :in slots)
               (collect (restore-slot-value slot record i)))
         slots)))))

(defun restore-all-slots (instance)
  "Restores all slots wihtout local side effects from the database."
  (bind (((values prefetched-slot-values prefetched-slots) (restore-prefetched-slots instance))
         (non-prefetched-slots (non-prefetched-slots-of (class-of instance))))
    (values (append prefetched-slot-values (mapcar #L(restore-slot instance !1) non-prefetched-slots))
            (append prefetched-slots non-prefetched-slots))))

(defun table-aliases-and-where-clause-for-instance (instance tables)
  (values
   (mapcar #L(sql-table-alias :name (name-of !1) :alias (name-of !1)) tables)
   (apply #'sql-and
          (sql-= (sql-column-alias :table (name-of (first tables)) :column +oid-id-column-name+)
                 (sql-literal :type +oid-id-sql-type+ :value (id-of instance)))
          (mapcar #L(sql-= (sql-column-alias :table (name-of (first tables)) :column +oid-id-column-name+)
                           (sql-column-alias :table (name-of !1) :column +oid-id-column-name+))
                  (rest tables)))))

;;;;;;;;;;;;;;;;;;;;;;
;;; RDBMS slot storers

(defun store-slot-value (slot slot-value rdbms-values index)
  "Provides convenient access to the arguments in the debugger."
  (declare (optimize (debug 3)))
  (funcall (writer-of slot) slot-value rdbms-values index))

(defun delete-slot-set (instance slot)
  (update-records (name-of (table-of slot))
		  (columns-of slot)
		  '(nil nil)
		  (id-column-matcher-where-clause instance (id-column-of slot))))

(defun store-slot-set (instance slot value)
  "Stores the non lazy list without local side effects into the database."
  (delete-slot-set instance slot)
  (when value
    (let ((rdbms-values (make-array +oid-column-count+)))
      (object-writer instance rdbms-values 0)
      (update-records (name-of (table-of slot))
                      (columns-of slot)
                      rdbms-values
                      (id-column-list-matcher-where-clause value)))))

(defun store-1-n-association-end-set (instance slot value)
  "Stores the non lazy list association end value without local side effects into the database."
  (store-slot-set instance slot value))

(defun delete-m-n-association-end-set (instance slot)
  (delete-records (name-of (table-of slot))
		  (id-column-matcher-where-clause instance (id-column-of slot))))

(defun insert-into-m-n-association-end-set (instance slot value)
  (bind ((other-slot (other-association-end-of slot))
         (rdbms-values (make-array (* 2 +oid-column-count+))))
    (object-writer value rdbms-values 0)
    (object-writer instance rdbms-values +oid-column-count+)
    (insert-record (name-of (table-of slot))
                   (append (columns-of slot) (columns-of other-slot))
                   rdbms-values)))

(defun store-m-n-association-end-set (instance slot value)
  "Stores the non lazy list association end value without local side effects into the database."
  (delete-m-n-association-end-set instance slot)
  (when value
    (mapc #L(insert-into-m-n-association-end-set instance slot !1) value)))

(defun store-slot (instance slot value)
  "Stores a single slot without local side effects into the database."
  (cond ((and (typep slot 'persistent-association-end-effective-slot-definition)
	      (eq (association-kind-of (association-of slot)) :1-1)
              (secondary-association-end-p slot))
         (when-bind other-instance (and (slot-boundp-using-class (class-of instance) instance slot)
                                        (slot-value-using-class (class-of instance) instance slot))
           (bind ((other-slot (other-effective-association-end-for (class-of other-instance) slot)))
             (store-slot other-instance other-slot nil)))
         (when (and value
                    (not (unbound-slot-value-p value)))
           (bind ((other-slot (other-effective-association-end-for (class-of value) slot)))
             (store-slot value other-slot instance))))
        ((and (typep slot 'persistent-association-end-effective-slot-definition)
	      (eq (association-kind-of (association-of slot)) :1-n)
	      (eq (cardinality-kind-of slot) :n))
	 (when (or value
                   (persistent-p instance))
           (store-1-n-association-end-set instance slot value)))
	((and (typep slot 'persistent-association-end-effective-slot-definition)
	      (eq (association-kind-of (association-of slot)) :m-n))
	 (when (or value
                   (persistent-p instance))
           (store-m-n-association-end-set instance slot value)))
	((set-type-p (normalized-type-of slot))
         (store-slot-set instance slot value))
	(t
         (when-bind columns (columns-of slot)
           (bind ((rdbms-values (make-array (length columns))))
             (store-slot-value slot value rdbms-values 0)
             (update-records (name-of (table-of slot))
                             columns
                             rdbms-values
                             (id-column-matcher-where-clause instance)))))))

(defun store-prefetched-slots (instance)
  "Stores all prefetched slots without local side effects into the database. Executes one insert statement for each table."
  (bind ((prefetched-slots (prefetched-slots-of (class-of instance)))
         (tables (delete-duplicates (mapcar #'table-of prefetched-slots)))
         (oid (oid-of instance)))    
    (dolist (table tables)
      (bind ((slots (collect-if #L(eq (table-of !1) table) prefetched-slots))
             (slot-values (mapcar #L(underlying-slot-boundp-or-value-using-class (class-of instance) instance !1) slots))
             (oid-columns (oid-columns-of table))
             (columns (mappend #'columns-of slots))
             (oid-values (oid->rdbms-values oid))
             (rdbms-values (make-array (length columns))))
        (iter (for slot :in slots)
              (for slot-value :in slot-values)
              (for index :initially 0 :then (+ index (length (columns-of slot))))
              (store-slot-value slot slot-value rdbms-values index))
        (if (persistent-p instance)
            (update-records (name-of table) columns rdbms-values (id-column-matcher-where-clause instance))
            (insert-record (name-of table) (append oid-columns columns) (concatenate 'vector oid-values rdbms-values)))))
    (unless (persistent-p instance)
      (dolist (table (set-difference (data-tables-of (class-of instance)) tables))
        (insert-record (name-of table) (oid-columns-of table) (oid->rdbms-values oid))))))

(defun store-all-slots (instance)
  "Stores all slots wihtout local side effects into the database."
  (store-prefetched-slots instance)
  (bind ((class (class-of instance)))
    (mapc #L(store-slot instance !1 (underlying-slot-boundp-or-value-using-class class instance !1))
          (non-prefetched-slots-of class))))

;;;;;;;;;;;
;;; Utility

(defun id-column-matcher-where-clause (instance &optional (id-name +oid-id-column-name+))
  (sql-binary-operator :name '=
                       :left (sql-identifier :name id-name)
                       :right (sql-literal :type +oid-id-sql-type+ :value (id-of instance))))

(defun id-column-list-matcher-where-clause (values &optional (id-name +oid-id-column-name+))
  (sql-binary-operator :name 'in
                       :left (sql-identifier :name id-name)
                       :right (mapcar (lambda (value)
                                        (sql-literal :type +oid-id-sql-type+ :value (id-of value)))
                                      values)))
