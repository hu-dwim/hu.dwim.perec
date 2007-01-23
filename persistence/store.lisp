(in-package :cl-perec)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RDBMS slot restorers

(defun slot-value-from-rdbms-values (object slot values)
  "Converts RDBMS values to slot values."
  (funcall (transformer-of (reader-of slot)) object slot values))

(defun restore-set (object slot)
  "Restores the non lazy list without local side effects from the database."
  (mapcar #L(slot-value-from-rdbms-values object slot !1)
          (select-records (oid-columns-of (table-of slot))
                          (list (name-of (table-of slot)))
                          (funcall (where-clause-of (reader-of slot)) object))))

(defun restore-1-n-association-end-set (object slot)
  "Restores the non lazy list association end value without local side effects from the database."
  (restore-set object slot))

(defun restore-m-n-association-end-set (object slot)
  "Restores the non lazy list association end value without local side effects from the database."
  (mapcar #L(slot-value-from-rdbms-values object slot !1)
          (select-records (columns-of slot)
                          (list (name-of (table-of slot)))
                          (funcall (where-clause-of (reader-of slot)) object))))

(defun restore-slot (object slot)
  "Restores a single slot without local side effects from the database."
  (values
   (cond ((and (typep slot 'persistent-association-end-effective-slot-definition)
               (eq (association-kind-of (association-of slot)) :1-n)
               (eq (cardinality-kind-of slot) :n))
          (restore-1-n-association-end-set object slot))
         ((and (typep slot 'persistent-association-end-effective-slot-definition)
               (eq (association-kind-of (association-of slot)) :m-n))
          (restore-m-n-association-end-set object slot))
         ((set-type-p (remove-null-and-unbound-if-or-type (slot-definition-type slot)))
          (restore-set object slot))
         (t
          (bind ((record
                  (first
                   (select-records (columns-of slot)
                                   (list (name-of (table-of slot)))
                                   (funcall (where-clause-of (reader-of slot)) object)))))
            (slot-value-from-rdbms-values object slot record))))
   slot))

(defun restore-prefetched-slots (object &optional (allow-missing #f))
  "Restores all prefetched slots at once without local side effects from the database. Executes a single select statement."
  (if-bind slots (prefetched-slots-of (class-of object))
    (bind ((tables (delete-duplicates (mapcar #L(table-of !1) slots)))
           (record
            (first
             (select-records (mapcan (lambda (slot)
                                       (mapcar (lambda (column)
                                                 (sql-column-alias :table (name-of (table-of slot)) :column column))
                                               (columns-of slot)))
                                     slots)
                             (mapcar #L(sql-table-alias :name (name-of !1) :alias (name-of !1)) tables)
                             (apply #'sql-and
                                    (sql-= (sql-column-alias :table (name-of (first tables)) :column +id-column-name+)
                                           (sql-literal :type +oid-id-sql-type+ :value (id-of object)))
                                    (mapcar #L(sql-= (sql-column-alias :table (name-of (first tables)) :column +id-column-name+)
                                                     (sql-column-alias :table (name-of !1) :column +id-column-name+))
                                            (rest tables)))))))
      (assert (or record allow-missing))
      (when record
        (values
         (iter (for i first 0 then (+ i (length (columns-of slot))))
               (for slot in slots)
               (collect (slot-value-from-rdbms-values
                         object
                         slot
                         (nthcdr i record))))
         slots)))))

(defun restore-all-slots (object)
  "Restores all slots wihtout local side effects from the database."
  (bind (((values prefetched-slot-values prefetched-slots) (restore-prefetched-slots object))
         (non-prefetched-slots (non-prefetched-slots-of (class-of object))))
    (values (append prefetched-slot-values (mapcar #L(restore-slot object !1) non-prefetched-slots))
            (append prefetched-slots non-prefetched-slots))))

;;;;;;;;;;;;;;;;;;;;;;
;;; RDBMS slot storers

(defun rdbms-values-from-slot-value (object slot value)
  "Convert slot values to RDBMS values."
  (funcall (transformer-of (writer-of slot)) object slot value))

(defun delete-set (object slot)
  (update-records (name-of (table-of slot))
		  (columns-of slot)
		  '(nil nil)
		  (oid-matcher-where-clause
                   object
                   (name-of (id-column-of (most-generic-other-effective-association-end-for slot))))))

(defun delete-1-n-association-end-set (object slot)
  (delete-set object slot))

(defun insert-into-set (object slot value)
  (update-records (name-of (table-of slot))
		  (columns-of slot)
		  (rdbms-values-from-slot-value object slot value)
		  (funcall (where-clause-of (writer-of slot)) object value)))

(defun insert-into-1-n-association-end-set (object slot value)
  (insert-into-set object slot value))

(defun store-set (object slot value)
  "Stores the non lazy list without local side effects into the database."
  (delete-set object slot)
  (when value
    (update-records (name-of (table-of slot))
                    (columns-of slot)
                    (rdbms-values-from-slot-value object slot value)
                    (list-matcher-writer-where-clause +id-column-name+ object value))))

(defun store-1-n-association-end-set (object slot value)
  "Stores the non lazy list association end value without local side effects into the database."
  (store-set object slot value))

(defun delete-m-n-association-end-set (object slot)
  (delete-records (name-of (table-of slot))
		  (funcall (where-clause-of (writer-of slot)) object object)))

(defun insert-into-m-n-association-end-set (object slot value)
  (bind ((other-slot (most-generic-other-effective-association-end-for slot)))
    (insert-records (name-of (table-of slot))
                    (append (columns-of other-slot) (columns-of slot))
                    (append
                     (rdbms-values-from-slot-value object slot value)
                     (rdbms-values-from-slot-value value slot object)))))

(defun store-m-n-association-end-set (object slot value)
  "Stores the non lazy list association end value without local side effects into the database."
  (delete-m-n-association-end-set object slot)
  (when value
    (mapc #L(insert-into-m-n-association-end-set object slot !1) value)))

(defun store-slot (object slot value)
  "Stores a single slot without local side effects into the database."
  (cond ((and (typep slot 'persistent-association-end-effective-slot-definition)
	      (eq (association-kind-of (association-of slot)) :1-n)
	      (eq (cardinality-kind-of slot) :n))
	 (when (or value
                   (persistent-p object))
           (store-1-n-association-end-set object slot value)))
	((and (typep slot 'persistent-association-end-effective-slot-definition)
	      (eq (association-kind-of (association-of slot)) :m-n))
	 (when (or value
                   (persistent-p object))
           (store-m-n-association-end-set object slot value)))
	((and (typep slot 'persistent-association-end-effective-slot-definition)
	      (eq (association-kind-of (association-of slot)) :1-1)
              (primary-association-end-p slot))
         (bind ((other-slot (most-generic-other-effective-association-end-for slot)))
           (when-bind other-object (slot-value-using-class (class-of object) object slot)
             (store-slot other-object other-slot nil))
           (when value
             (store-slot value other-slot object))))
        ((set-type-p (remove-null-and-unbound-if-or-type (slot-definition-type slot)))
         (store-set object slot value))
	(t
         (update-records (name-of (table-of slot))
                         (columns-of slot)
                         (rdbms-values-from-slot-value object slot value)
                         (funcall (where-clause-of (writer-of slot)) object value)))))

(defun store-prefetched-slots (object)
  "Stores all prefetched slots without local side effects into the database. Executes one insert statement for each table."
  (bind ((prefetched-slots (prefetched-slots-of (class-of object)))
         (tables (delete-duplicates (mapcar #L(table-of !1) prefetched-slots))))    
    (dolist (table tables)
      (bind ((slots (remove-if-not #L(eq (table-of !1) table) prefetched-slots))
             (slot-values (mapcar #L(slot-value-using-class (class-of object) object !1) slots))
             (oid-columns (oid-columns-of table))
             (columns (mappend #'columns-of slots))
             (oid-values (oid-values object))
             (rdbms-values (mappend #L(rdbms-values-from-slot-value object !1 !2) slots slot-values)))
        (if (persistent-p object)
            (update-records (name-of table) columns rdbms-values (oid-matcher-reader-where-clause object))
            (insert-records (name-of table) (append oid-columns columns) (append oid-values rdbms-values)))))
    (unless (persistent-p object)
      (dolist (table (set-difference (data-tables-of (class-of object)) tables))
        (insert-records (name-of table) (oid-columns-of table) (oid-values object))))))

(defun store-all-slots (object)
  "Stores all slots wihtout local side effects into the database."
  (store-prefetched-slots object)
  (mapc #L(store-slot object !1 (slot-value-using-class (class-of object) object !1))
        (non-prefetched-slots-of (class-of object))))
