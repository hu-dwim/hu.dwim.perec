(in-package :cl-perec)

;;;;;;;;;;;;;
;;; Constants

(defparameter *lazy-collections* #f
  "True means slot-value-using-class will by default return lazy collections.")

(eval-always
  (unless (fboundp 'make-unbound-marker)
    (defstruct unbound-marker
      "This structure is used for the unbound slot value marker. The type for that marker must be a subtype of t and cannot be a subtype of any other type.")))

(define-constant +unbound-slot-marker+ (make-unbound-marker)
  :test equalp
  :documentation "This value is used to signal unbound slot value returned from database.")

(defmethod make-load-form ((instance unbound-marker) &optional environment)
  (declare (ignore environment))
  '(make-unbound-marker))

(def (function io) unbound-slot-marker-p (value)
  (eq +unbound-slot-marker+ value))

;;;;;;;;;;;
;;; Utility

(def (function io) object-reader (rdbms-values index)
  (load-instance (rdbms-values->oid* rdbms-values index) :skip-existence-check #t))

(def (function io) object-writer (slot-value rdbms-values index)
  (oid->rdbms-values* (oid-of slot-value) rdbms-values index))

(def (function io) id-column-matcher-where-clause (instance &optional (id-name +oid-id-column-name+))
  (sql-binary-operator :name '=
                       :left (sql-identifier :name id-name)
                       :right (sql-literal :type +oid-id-sql-type+ :value (id-of instance))))

(def (function io) id-column-list-matcher-where-clause (values &optional (id-name +oid-id-column-name+))
  (sql-binary-operator :name 'in
                       :left (sql-identifier :name id-name)
                       :right (mapcar (lambda (value)
                                        (sql-literal :type +oid-id-sql-type+ :value (id-of value)))
                                      values)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RDBMS slot restorers

(def (function io) restore-slot-value (slot rdbms-values index)
  "Provides convenient access to the arguments in the debugger."
  (funcall (the function (reader-of slot)) rdbms-values index))

(def (function o) restore-slot-set (instance slot)
  "Restores the non lazy list without local side effects from the database."
  (map 'list #L(object-reader !1 0)
       (select-records (oid-columns-of (table-of slot))
                       (list (name-of (table-of slot)))
                       (id-column-matcher-where-clause instance (id-column-of slot))
                       (let ((type (slot-definition-type slot)))
                         (if (ordered-set-type-p type)
                             ;; TODO: use reflection instead of third
                             (list (sql-identifier :name (rdbms-name-for (third type)))))))))

(def (function o) restore-1-n-association-end-set (instance slot)
  "Restores the non lazy list association end value without local side effects from the database."
  (restore-slot-set instance slot))

(def (function o) restore-m-n-association-end-set (instance slot)
  "Restores the non lazy list association end value without local side effects from the database."
  (bind ((other-slot (other-association-end-of slot)))
    (map 'list #L(object-reader !1 0)
         (select-records (columns-of slot)
                         (list (name-of (table-of slot)))
                         (id-column-matcher-where-clause instance (id-column-of other-slot))))))

(def (function o) restore-slot (instance slot)
  "Restores a single slot without local side effects from the database."
  (values
   (cond ((and (typep slot 'persistent-association-end-effective-slot-definition)
               (eq (association-kind-of (association-of slot)) :1-1)
               (secondary-association-end-p slot))
          (bind ((records
                  (select-records +oid-column-names+
                                  (list (name-of (table-of slot)))
                                  (sql-= (sql-literal :type +oid-id-sql-type+ :value (id-of instance))
                                         (sql-identifier :name (id-column-of slot))))))
            (declare (type vector records))
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

(def (function o) restore-prefetched-slots (instance &optional (allow-missing #f))
  "Restores all prefetched slots at once without local side effects from the database. Executes a single select statement."
  (if-bind slots (prefetched-slots-of (class-of instance))
    (bind ((tables (delete-duplicates (mapcar #'table-of slots)))
           ((values table-aliases where-clause) (table-aliases-and-where-clause-for-instance (id-of instance) tables))
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
                     (assert (= 1 (length records)) nil "The persistent instance ~A is missing from the database" instance)
                     (elt-0 records))))
      (declare (type vector records))
      (declare (type (or null vector) record))
      (when record
        (values
         (iter (for (the fixnum i) :first 0 :then (the fixnum (+ i (length (columns-of slot)))))
               (for slot :in slots)
               (collect (restore-slot-value slot record i)))
         slots)))))

;; TODO:
;; (funcall (compile-restore-prefetched-slots (class-of instance)) instance allow-missing)
(def function compile-restore-prefetched-slots (class)
  "Restores all prefetched slots at once without local side effects from the database. Executes a single select statement."
  (ensure-finalized class)
  (compile nil
   `(lambda (instance &optional (allow-missing #f))
     ,(when-bind slots (prefetched-slots-of class)
                 `(bind ((records
                          (execute
                           ,(bind ((tables (delete-duplicates (mapcar #'table-of slots)))
                                   ((values table-aliases where-clause)
                                    (table-aliases-and-where-clause-for-instance (sql-unquote :form '(id-of instance)) tables)))
                                  (rdbms::expand-sql-ast-into-lambda-form
                                   (make-instance 'sql-select
                                                  :columns (mapcan (lambda (slot)
                                                                     (mapcar (lambda (column)
                                                                               (sql-column-alias :table (name-of (table-of slot)) :column column))
                                                                             (columns-of slot)))
                                                                   slots)
                                                  :tables table-aliases
                                                  :where where-clause)))))
                         (record (unless (and allow-missing
                                              (zerop (length records)))
                                   (debug-only (assert (= 1 (length records))))
                                   (elt-0 records))))
                   (declare (type vector records))
                   (declare (type (or null vector) record))
                   (when record
                     (values
                      (list
                       ,@(iter (for i :first 0 :then (+ i (length (columns-of slot))))
                               (for slot :in slots)
                               (collect `(funcall ,(reader-of slot) record ,i))))
                      ',slots)))))))

(def (function o) restore-all-slots (instance)
  "Restores all slots wihtout local side effects from the database."
  (bind (((values prefetched-slot-values prefetched-slots) (restore-prefetched-slots instance))
         (non-prefetched-slots (non-prefetched-slots-of (class-of instance))))
    (values (append prefetched-slot-values (mapcar #L(restore-slot instance !1) non-prefetched-slots))
            (append prefetched-slots non-prefetched-slots))))

(def (function o) table-aliases-and-where-clause-for-instance (instance-id tables)
  (values
   (mapcar #L(sql-table-alias :name (name-of !1) :alias (name-of !1)) tables)
   (apply #'sql-and
          (sql-= (sql-column-alias :table (name-of (first tables)) :column +oid-id-column-name+)
                 (sql-literal :type +oid-id-sql-type+ :value instance-id))
          (mapcar #L(sql-= (sql-column-alias :table (name-of (first tables)) :column +oid-id-column-name+)
                           (sql-column-alias :table (name-of !1) :column +oid-id-column-name+))
                  (rest tables)))))

;;;;;;;;;;;;;;;;;;;;;;
;;; RDBMS slot storers

(def (function io) store-slot-value (slot slot-value rdbms-values index)
  "Provides convenient access to the arguments in the debugger."
  (funcall (the function (writer-of slot)) slot-value rdbms-values index))

(def (function o) delete-slot-set (instance slot)
  (update-records (name-of (table-of slot))
		  (columns-of slot)
		  '(nil nil)
		  (id-column-matcher-where-clause instance (id-column-of slot))))

(def (function o) store-slot-set (instance slot values)
  "Stores the non lazy list without local side effects into the database."
  (delete-slot-set instance slot)
  (when values
    #+nil
    (dolist (value values)
      (check-slot-value-type instance slot value))
    (let ((rdbms-values (make-array +oid-column-count+)))
      (object-writer instance rdbms-values 0)
      (update-records (name-of (table-of slot))
                      (columns-of slot)
                      rdbms-values
                      (id-column-list-matcher-where-clause values)))))

(def (function o) store-1-n-association-end-set (instance slot value)
  "Stores the non lazy list association end value without local side effects into the database."
  (store-slot-set instance slot value))

(def (function o) delete-m-n-association-end-set (instance slot)
  (delete-records (name-of (table-of slot))
		  (id-column-matcher-where-clause instance (id-column-of slot))))

(def (function o) insert-into-m-n-association-end-set (instance slot value)
  (check-slot-value-type instance slot value)
  (bind ((other-slot (other-association-end-of slot))
         (rdbms-values (make-array (* 2 +oid-column-count+))))
    (object-writer value rdbms-values 0)
    (object-writer instance rdbms-values +oid-column-count+)
    (insert-record (name-of (table-of slot))
                   (append (columns-of slot) (columns-of other-slot))
                   rdbms-values)))

(def (function o) store-m-n-association-end-set (instance slot value)
  "Stores the non lazy list association end value without local side effects into the database."
  (delete-m-n-association-end-set instance slot)
  (when value
    (mapc #L(progn
              (check-slot-value-type instance slot !1)
              (insert-into-m-n-association-end-set instance slot !1))
          value)))

(def (function o) store-slot (instance slot value)
  "Stores a single slot without local side effects into the database."
  (cond ((and (typep slot 'persistent-association-end-effective-slot-definition)
	      (eq (association-kind-of (association-of slot)) :1-1)
              (secondary-association-end-p slot))
         (check-slot-value-type instance slot value)
         (when-bind other-instance (and (persistent-p instance)
                                        (slot-boundp-using-class (class-of instance) instance slot)
                                        (slot-value-using-class (class-of instance) instance slot))
           (bind ((other-slot (other-effective-association-end-for (class-of other-instance) slot)))
             (store-slot other-instance other-slot nil)))
         (when (and value
                    (not (unbound-slot-marker-p value)))
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
         (check-slot-value-type instance slot value)
         (when-bind columns (columns-of slot)
           (bind ((rdbms-values (make-array (length (the list columns)))))
             (store-slot-value slot value rdbms-values 0)
             (update-records (name-of (table-of slot))
                             columns
                             rdbms-values
                             (id-column-matcher-where-clause instance)))))))

(def (function o) store-prefetched-slots (instance)
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
        (declare (type list slots slot-values columns))
        (iter (for slot :in slots)
              (for slot-value :in slot-values)
              (for index :initially 0 :then (the fixnum (+ index (length (columns-of slot)))))
              (check-slot-value-type instance slot slot-value)
              (store-slot-value slot slot-value rdbms-values index))
        (if (persistent-p instance)
            (update-records (name-of table) columns rdbms-values (id-column-matcher-where-clause instance))
            (insert-record (name-of table) (append oid-columns columns) (concatenate 'vector oid-values rdbms-values)))))
    (unless (persistent-p instance)
      (dolist (table (set-difference (data-tables-of (class-of instance)) tables))
        (insert-record (name-of table) (oid-columns-of table) (oid->rdbms-values oid))))))

(def (function o) store-all-slots (instance)
  "Stores all slots wihtout local side effects into the database."
  (store-prefetched-slots instance)
  (bind ((class (class-of instance)))
    (mapc #L(store-slot instance !1 (underlying-slot-boundp-or-value-using-class class instance !1))
          (non-prefetched-slots-of class))))
