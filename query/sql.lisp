;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;
;;; SQL syntax
;;;

;;;----------------------------------------------------------------------------
;;; Selects

(defun sql-select-oids-for-class (class-name)
  "Generates a select for the oids of instances of the class named CLASS-NAME."
  (bind ((class (find-class class-name)))
    (ensure-class-and-subclasses-exported class)
    (sql-select-oids-from-table (primary-relation-of class))))

(defun sql-select-oids-from-table (thing)
  "Generates a select for the oids in THING."
  (sql-select :columns +oid-column-names+ :tables (list (sql-table-reference-for thing nil))))

;;;----------------------------------------------------------------------------
;;; Deletes

(defun sql-delete-from-tables (data-tables joined-tables where &optional qualifier)
  (bind ((temp-table (rdbms-name-for 'deleted-ids :table)))
    (values
      (rdbms::expand-sql-ast-into-lambda-form
        (create-temporary-table temp-table
                                (list
                                 (sql-column :name +oid-id-column-name+ :type +oid-id-sql-type+))
                                (sql-select
                                  :columns (list (sql-id-column-reference-for qualifier))
                                  :tables  joined-tables
                                  :where where)
                                *database*))
      (mapcar (lambda (table)
                (rdbms::expand-sql-ast-into-lambda-form
                 (sql-delete-for-subselect table
                                           (sql-subquery
                                             :query
                                             (sql-select
                                               :columns (list +oid-id-column-name+)
                                               :tables (list temp-table))))))
              data-tables)
      (drop-temporary-table temp-table *database*))))

(defun sql-delete-for-subselect (table subselect)
  "Generate a delete command for records in TABLE whose oid is in the set returned by SUBSELECT."
  (sql-delete-from-table
   table
   :where (sql-in (sql-id-column-reference-for table) subselect)))

(defun sql-delete-from-table (table &key where)
  "Generate a delete command for records in TABLE that satisfies the WHERE clause."
  (sql-delete
    :table (sql-table-reference-for table nil)
    :where where))

(defun tables-for-delete (class)
  "Returns the tables where instances of CLASS are stored."
  (bind ((super-classes (persistent-effective-super-classes-of class))
         (sub-classes (persistent-effective-sub-classes-of class))
         (class-primary-table (primary-table-of class))
         (super-primary-tables (mapcar 'primary-table-of super-classes))
         (super-sub-primary-tables (mappend 'data-tables-of sub-classes)))
    (delete nil
            (delete-duplicates
             (append
              (list class-primary-table)
              super-primary-tables
              super-sub-primary-tables)))))

;;;----------------------------------------------------------------------------
;;; Updates
(defun sql-update-tables (table->column-value-pairs table-refs where &optional qualifier)
  (bind ((temp-table (rdbms-name-for 'updated-ids :table)))
    (values
     (rdbms::expand-sql-ast-into-lambda-form
      (create-temporary-table temp-table
                              (list (sql-column :name +oid-id-column-name+ :type +oid-id-sql-type+))
                              (sql-select
                                :columns (list (sql-id-column-reference-for qualifier))
                                :tables  table-refs
                                :where where)
                              *database*))
     (maphash (lambda (table column-value-pairs)
                (rdbms::expand-sql-ast-into-lambda-form
                 (sql-update-for-subselect table
                                           (mapcar #'first column-value-pairs)
                                           (mapcar #'second column-value-pairs)
                                           (sql-subquery
                                             :query
                                             (sql-select
                                               :columns (list +oid-id-column-name+)
                                               :tables (list temp-table))))))
              table->column-value-pairs)
     (drop-temporary-table temp-table *database*))))

(defun sql-update-for-subselect (table columns values subselect)
  "Generate a delete command for records in TABLE whose oid is in the set returned by SUBSELECT."
  (sql-update-table
   table columns values
   :where (sql-in (sql-id-column-reference-for table) subselect)))

(defun sql-update-table (table columns values &key where)
  (sql-update
    :table (sql-table-reference-for table nil)
    :columns (sql-column-references-for columns nil)
    :values values
    :where where))

;;;----------------------------------------------------------------------------
;;; Temporary tables
(defgeneric create-temporary-table (table-name columns subselect database)
  (:method (table-name columns subselect database)
    (sql-create-table
      :name table-name
      :temporary #t
      :columns (mapcar (lambda (column) (sql-identifier :name (slot-value column 'cl-rdbms::name))) columns)
      :as (sql-subquery :query subselect)))

  (:method (table-name columns subselect (database oracle))
    (ensure-oracle-temporary-table-exists table-name columns)
    (sql-insert
       :table table-name
       :columns columns
       :subselect (sql-subquery :query subselect))))

(defun ensure-oracle-temporary-table-exists (table-name columns)
  (unless (oracle-temporary-table-exists-p table-name)
    (with-transaction (execute (sql-create-table
                                :name table-name
                                :temporary :delete-rows
                                :columns columns)))))

(defun oracle-temporary-table-exists-p (table-name)
  (execute (format nil
                   "select table_name from user_tables where lower(table_name)='~A' and temporary='Y'"
                   (string-downcase (rdbms-name-for table-name))) ;; TODO should be case sensitive
           :result-type 'list))

(defgeneric drop-temporary-table (table-name database)
  (:method (table-name database)
           (sql-drop-table :name table-name))

  (:method (table-name (database oracle))
           nil))

;;;----------------------------------------------------------------------------
;;; Alias names

(defvar *suppress-alias-names* nil)

(defgeneric sql-alias-for (element)
  (:documentation "Generates a table alias for the given ELEMENT. Alias names may be supressed
by setting *SUPRESS-ALIAS-NAMES* to true.")
  (:method ((name symbol))
           (unless *suppress-alias-names*
             (rdbms-name-for name)))
  (:method ((variable query-variable))
           (sql-alias-for (name-of variable)))
  (:method ((class persistent-class))
           (sql-alias-for (class-name class)))
  (:method ((table table))
           (unless *suppress-alias-names*
             (name-of table))))

;;;----------------------------------------------------------------------------
;;; Select lists

(defgeneric sql-select-list-for (element)
  (:method ((query query))
    ;; select oids and prefetched slots
    (mapcan
     (lambda (variable) (sql-columns-for-variable variable (prefetch-mode-of query)))
     (query-variables-of query))))

(defun sql-columns-for-variable (variable prefetch-mode)
  (bind ((table-alias (sql-alias-for variable)))
    (append
     (sql-oid-column-references-for variable)
     (mapcan #L(sql-column-references-for !1 table-alias)
             (prefetched-slots-for variable prefetch-mode)))))

(defun prefetched-slots-for (variable prefetch-mode)
  (ecase prefetch-mode
    (:none nil)
    (:accessed
     (bind ((type (persistent-type-of variable)))
       (when (persistent-class-p type)
         (collect-if #L(eq (table-of !1) (primary-table-of type))
                     (prefetched-slots-of type)))))
    (:all
     (bind ((type (persistent-type-of variable)))
       (when (persistent-class-p type)
         (prefetched-slots-of type))))))

;;;----------------------------------------------------------------------------
;;; Table references

(defgeneric sql-table-references-for (element)
  (:method ((query query))
           (bind ((variables (query-variables-of query)))
             (delete nil (mapcar 'sql-table-reference-for variables variables)))))

(defgeneric sql-table-reference-for (element alias)
  (:method ((table sql-table-alias) (alias null))
    table)
  
  (:method ((table table) (alias symbol))
    (ensure-exported table)
    (sql-table-alias :name (name-of table) :alias alias))

  (:method ((view view) (alias symbol))
    (ensure-exported view)
    (sql-table-alias :name (name-of view) :alias alias))

  (:method ((subquery sql-subquery) (alias symbol))
    (sql-derived-table :subquery subquery :alias (or alias (gensym "pg")))) ; Postgresql requires alias

  (:method ((class persistent-class) (alias symbol))
    (sql-table-reference-for-type class alias))

  (:method ((class-name symbol) (alias symbol))
    (aif (find-class class-name)
         (sql-table-reference-for it alias)
         (error "No persistent class named '~A~%" class-name)))

  (:method ((variable query-variable) (alias symbol))
    (assert (not (eq (persistent-type-of variable) +unknown-type+)))
    (sql-table-reference-for-type
     `(join ,(persistent-type-of variable) ,@(joined-types-of variable))
     alias))

  (:method ((association persistent-association) (alias symbol))
    (assert (eq (association-kind-of association) :m-n))
    (ensure-exported association)
    (sql-table-reference-for (primary-table-of association) alias))

  (:method ((syntax syntax-object) (alias symbol))
    (make-function-call :fn 'sql-table-reference-for :args (list syntax alias)))

  (:method (element (variable query-variable))
    (sql-table-reference-for element (sql-alias-for variable)))

  (:method (element (class persistent-class))
    (sql-table-reference-for element (sql-alias-for class)))

  (:method (element (syntax syntax-object))
    (make-function-call :fn 'sql-table-reference-for :args (list element syntax))))

(def function sql-table-reference-for-type (type &optional alias)
  (sql-table-reference-for-type* (simplify-persistent-class-type type) alias))

(defgeneric sql-table-reference-for-type* (type &optional alias)
  
  (:method ((class persistent-class) &optional alias)
           (ensure-class-and-subclasses-exported class)
           (when-bind relation (primary-relation-of class)
             (sql-table-reference-for relation alias)))

  (:method ((type-name symbol) &optional alias)
           (bind ((class (find-class type-name #f)))
             (typecase class
               (persistent-class (sql-table-reference-for-type* class alias))
               (otherwise nil)))) ; FIXME signal error

  (:method ((type syntax-object) &optional alias) ;; type unknown at compile time
    (sql-unquote :form `(sql-table-reference-for-type ,(backquote-type-syntax type) ',alias)))

  (:method ((combined-type list) &optional alias)
           (if (contains-syntax-p combined-type)
               ;; delay evaluation until run-time
               (sql-unquote :form `(sql-table-reference-for-type ,(backquote-type-syntax combined-type) ',alias))
               ;; and/or/not types
               (labels ((ensure-sql-query (table-ref)
                          (assert (not (syntax-object-p table-ref)))
                          (etypecase table-ref
                            (sql-table-alias (sql-subquery :query (sql-select-oids-from-table table-ref)))
                            (sql-derived-table (cl-rdbms::subquery-of table-ref)))) ; TODO missing export
                        (ensure-alias (table-ref)
                          (if (or (typep table-ref 'sql-table-alias)
                                  (typep table-ref 'sql-derived-table))
                              (progn
                                (setf (cl-rdbms::alias-of table-ref) alias)
                                table-ref)
                              (sql-derived-table
                                :subquery table-ref
                                :alias alias)))
                        (combine-types (sql-set-operation types)
                          (bind ((operands (delete nil
                                                   (mapcar 'sql-table-reference-for-type* types))))
                            (case (length operands)
                              (0 nil)
                              (1 (ensure-alias (first operands)))
                              (t (ensure-alias
                                  (sql-subquery
                                    :query (apply sql-set-operation
                                                  (mapcar #'ensure-sql-query operands))))))))
                        (join-types (types)
                          (bind ((primary-table-ref (sql-table-reference-for-type*
                                                     (first types) (gensym "x")))
                                 (joined-table-refs
                                  (mapcan
                                   #L(when (primary-table-of !1)
                                       (list (sql-table-reference-for (primary-table-of !1) nil)))
                                   (rest types))))
                            (cond
                              ((null primary-table-ref) nil)
                              ((null joined-table-refs) (ensure-alias primary-table-ref))
                              (t (ensure-alias
                                  (reduce #L(sql-joined-table
                                              :kind :inner
                                              :left !1
                                              :right !2
                                              :using (list +oid-id-column-name+))
                                          joined-table-refs
                                          :initial-value primary-table-ref)))))))
                 (case (car combined-type)
                   (or (combine-types 'sql-union (rest combined-type)))
                   (and (combine-types 'sql-intersect (rest combined-type)))
                   (not (not-yet-implemented))
                   (join (join-types (rest combined-type)))
                   (t (error "Unsupported type constructor in ~A" combined-type)))))))

;;;----------------------------------------------------------------------------
;;; Column references

(defgeneric sql-column-reference-for (element qualifier)
  (:method ((column-name symbol) (qualifier symbol))
           (sql-column-alias :column column-name :table qualifier))

  (:method ((column column) qualifier)
           (sql-column-reference-for (rdbms::name-of column) qualifier))

  (:method ((association-end persistent-association-end-slot-definition) qualifier)
           (sql-column-reference-for (id-column-of association-end) qualifier))

  (:method ((slot persistent-slot-definition) qualifier)
           (bind ((column-names (column-names-of slot)))
             (ecase (length column-names)
               (1 (sql-column-reference-for (first column-names) qualifier))
               (2 (values
                   (sql-column-reference-for (second column-names) qualifier)
                   (sql-column-reference-for (first column-names) qualifier))))))

  (:method (element qualifier)
           (sql-column-reference-for element (sql-alias-for qualifier)))

  (:method ((column-alias sql-column-alias) qualifier)
           (sql-column-reference-for (rdbms::column-of column-alias) qualifier)))

(defun sql-id-column-reference-for (qualifier)
  (sql-column-reference-for +oid-id-column-name+ qualifier))

(defun sql-tag-column-reference-for (slot qualifier)
  (sql-column-reference-for (tag-column-of slot) qualifier))

(defgeneric sql-column-references-for (element qualifier)
  (:method ((column-names sequence) qualifier)
           (map 'list #L(sql-column-reference-for !1 qualifier) column-names))

  (:method ((slot persistent-slot-definition) qualifier)
           (sql-column-references-for (column-names-of slot) qualifier)))

(defun sql-oid-column-references-for (qualifier)
  (sql-column-references-for +oid-column-names+ qualifier))

;;;----------------------------------------------------------------------------
;;; Subselects

(defun sql-exists-subselect-for-variable (variable type)
  "Returns an sql expression which evaluates to true iff the query variable named VARIABLE-NAME
 has the type TYPE."
  (bind ((table-ref (sql-table-reference-for-type type type)))
    (if table-ref
        (sql-exists
         (sql-subquery
           :query
           (sql-select
             :columns (list 1)
             :tables (list table-ref)
             :where (sql-join-condition-for variable type nil))))
        (sql-false-literal))))

(defun sql-exists-subselect-for-association-end (variable association-end &optional class)
  "Returns an sql expression which evaluates to true iff the query variable VARIABLE
 has associated objects through ASSOCIATION-END with class CLASS."
  (bind ((class (or class (slot-definition-class (other-association-end-of association-end))))
         (table-ref (sql-table-reference-for class (sql-alias-for class))))
    (if table-ref
        (sql-exists
         (sql-subquery
           :query
           (sql-select
             :columns (list 1)
             :tables (list table-ref)
             :where (sql-join-condition-for variable class (other-association-end-of association-end)))))
        (sql-false-literal))))

(defun sql-aggregate-subselect-for-variable (aggregate-function n-association-end 1-var)
  (bind ((1-association-end (other-association-end-of n-association-end))
         (n-class (slot-definition-class 1-association-end))
         (n-var (make-query-variable :name (gensym (symbol-name (class-name n-class)))
                                     :persistent-type n-class))
         (table-ref (sql-table-reference-for n-var n-var)))
    (cond
      (table-ref
       (sql-subquery
         :query
         (sql-select
           :columns (list (funcall aggregate-function (sql-id-column-reference-for n-var)))
           :tables (list table-ref)
           :where (sql-join-condition-for 1-var n-var 1-association-end))))
      ((eq aggregate-function 'sql-count)
       (sql-literal :value 0))
      (t
       (sql-null-literal)))))

(defun sql-aggregate-subselect-for-m-n-association-end (aggregate-function association-end variable)
  (bind ((other-end (other-association-end-of association-end))
         (association (association-of association-end))
         (table (primary-table-of association)))
    (sql-subquery
      :query
      (sql-select
        :columns (list (funcall aggregate-function (sql-column-reference-for association-end table)))
        :tables (list (sql-table-reference-for association (name-of table)))
        :where (sql-=
                (sql-column-reference-for other-end (name-of table))
                (sql-id-column-reference-for variable))))))

(defun sql-subselect-for-secondary-association-end (association-end variable)
  (bind ((primary-association-end (other-association-end-of association-end))
         (class (slot-definition-class primary-association-end))
         (table-ref (sql-table-reference-for class nil)))
    (if table-ref
        (sql-subquery
          :query
          (sql-select
            :columns (list (sql-id-column-reference-for nil))
            :tables (list table-ref)
            :where (sql-=
                    (sql-column-reference-for primary-association-end nil)
                    (sql-id-column-reference-for variable))))
        (sql-null-literal))))           ; FIXME

(defun sql-subselect-for-m-n-association (association-end variable)
  (bind ((other-end (other-association-end-of association-end))
         (association (association-of association-end))
         (table (primary-table-of association)))
    (sql-subquery
      :query
      (sql-select
        :columns (list (sql-column-reference-for association-end table))
        :tables (list (sql-table-reference-for association (name-of table)))
        :where (sql-=
                (sql-column-reference-for other-end (name-of table))
                (sql-id-column-reference-for variable))))))



;;;----------------------------------------------------------------------------
;;; Joins

(defun sql-join-condition-for (object-1 object-2 association-end-2)
  (if (not association-end-2)
      (sql-=
        (sql-id-column-reference-for object-1)
        (sql-id-column-reference-for object-2))
      (bind ((association (association-of association-end-2))
             (association-kind (association-kind-of association)))
        (case association-kind
          (:1-1
           (if (primary-association-end-p association-end-2)
               (sql-=
                (sql-id-column-reference-for object-1)
                (sql-column-reference-for association-end-2 object-2))
               (sql-=
                (sql-id-column-reference-for object-2)
                (sql-column-reference-for (other-association-end-of association-end-2) object-1))))
          (:1-n
           (if (to-one-association-end-p association-end-2) ; TODO should check if primary
               (sql-=
                (sql-id-column-reference-for object-1)
                (sql-column-reference-for association-end-2 object-2))
               (sql-=
                (sql-id-column-reference-for object-2)
                (sql-column-reference-for (other-association-end-of association-end-2) object-1))))
          (:m-n
           (bind ((table (primary-table-of association)))
             (sql-and
              (sql-=
               (sql-id-column-reference-for object-1)
               (sql-column-reference-for association-end-2 table))
              (sql-=
               (sql-id-column-reference-for object-2)
               (sql-column-reference-for (other-association-end-of association-end-2) table)))))))))

(defun sql-join-condition-for-joined-variable (variable)
  (sql-join-condition-for variable (object-of variable) (association-end-of variable)))

(defun sql-join-condition-for-m-n-association (object-1 object-2 association-end-2)
  (bind ((table (primary-table-of (association-of association-end-2))))
    (sql-exists
     (sql-subquery
       :query
       (sql-select
         :columns (list 1)
         :tables (list (sql-identifier :name (name-of table)))
         :where (sql-join-condition-for object-1 object-2 association-end-2))))))

;;;----------------------------------------------------------------------------
;;; Boundp check
;;;
(defgeneric sql-slot-boundp (variable slot)

  (:method ((variable query-variable) (slot persistent-effective-slot-definition))
    (bind ((slot-type (canonical-type-for (slot-definition-type slot))))
      (cond
        ((tagged-type-p slot-type)
         (sql-<> (sql-tag-column-reference-for slot variable)
                 (sql-literal :value 1)))
        ((unbound-subtype-p slot-type)
         (sql-is-not-null (sql-column-reference-for slot variable)))
        (t ;; TODO: should be handled by partial eval
         (sql-true-literal))))))

(defgeneric sql-slot-is-null (variable slot)

  (:method ((variable query-variable) (slot persistent-effective-slot-definition))
           (bind ((slot-type (slot-definition-type slot)))
             (cond
               ((tagged-type-p slot-type)
                (sql-and
                 ;; TODO: this is #t for now
                 ;;(sql-is-not-null (sql-tag-column-reference-for slot variable))
                 (sql-tag-column-reference-for slot variable)
                 (sql-is-null (sql-column-reference-for slot variable))))
               ((null-subtype-p slot-type)
                (sql-is-null (sql-column-reference-for slot variable)))
               (t                                ;; TODO: partial eval
                (sql-false-literal))))))

;;;----------------------------------------------------------------------------
;;; Operators

(defun chained-operator (binary-operator &optional default)
  (lambda (first-arg &rest more-args)
    (if (null more-args)
        default
        (apply 'sql-and
               (iter (for arg in more-args)
                     (for prev-arg previous arg initially first-arg)
                     (collect (funcall binary-operator prev-arg arg)))))))

(defun pairwise-operator (binary-operator &optional default)
  (lambda (first-arg &rest more-args)
    (if (null more-args)
        default
        (apply 'sql-and
               (iter outer (for rest-args on more-args)
                     (for first previous (car rest-args) initially first-arg)
                     (iter (for second in rest-args)
                           (in outer (collect (funcall binary-operator first second)))))))))

(defun sql-equal (sql-expr-1 sql-expr-2 &key unbound-check-1 unbound-check-2 null-check-1 null-check-2
                  null-tag-1 null-tag-2)

  "Generates an equality test for the two sql expression and the corresponding boundness checks.
If one of the values is unbound, the test yields NULL, otherwise it yields true or false (two NULL
value is equal, when they represent the NIL lisp value)."
  (flet ((sql-is-null (sql unbound-check null-check null-tag-1 null-tag-2)
           (cond
             ((sql-null-literal-p sql)
              (sql-= null-tag-1 null-tag-2))
             ((and unbound-check null-check)
              (sql-if unbound-check
                      (sql-null-literal)
                      (sql-and null-check (sql-= null-tag-1 null-tag-2))))
             (unbound-check
              (sql-if unbound-check
                      (sql-null-literal)
                      (sql-false-literal)))
             (null-check
              (sql-and null-check (sql-= null-tag-1 null-tag-2)))
             (t
              (sql-false-literal))))
         (wrap-with-null-check (eq-check)
           (cond
             ((and null-check-1 null-check-2)
              (sql-if (sql-or null-check-1 null-check-2)
                      (sql-= null-tag-1 null-tag-2)
                      eq-check))
             (null-check-1
              (sql-and (sql-not null-check-1) eq-check))
             (null-check-2
              (sql-and (sql-not null-check-2) eq-check))
             (t
              eq-check)))
         (wrap-with-unbound-check (eq-check)
           (cond
             ((and unbound-check-1 unbound-check-2)
              (sql-if (sql-or unbound-check-1 unbound-check-2)
                      (sql-null-literal)
                      eq-check))
             (unbound-check-1
              (sql-if unbound-check-1
                      (sql-null-literal)
                      eq-check))
             (unbound-check-2
              (sql-if unbound-check-2
                      (sql-null-literal)
                      eq-check))
             (t
              eq-check))))
    (cond
      ((sql-null-literal-p sql-expr-1)
       (sql-is-null sql-expr-2 unbound-check-2 null-check-2 null-tag-2 null-tag-1))
      ((sql-null-literal-p sql-expr-2)
       (sql-is-null sql-expr-1 unbound-check-1 null-check-1 null-tag-1 null-tag-2))
      (t
       (wrap-with-unbound-check
        (wrap-with-null-check
         (sql-= sql-expr-1 sql-expr-2)))))))

(defun sql-subseq (seq start &optional end)
  "TODO: other sequnce types"
  (cond
    ((and (numberp start) (= start 0) (null end)) seq)
    ((null end) (sql-substring seq (sql-+ start 1) (sql-- (sql-length seq) start)))
    (t (sql-substring seq (sql-+ start 1) (sql-- end start)))))

(defun sql-substring (str start length)
  (sql-function-call :name "substr" :arguments (list str start length)))

(defun sql-boolean->boolean (val)
  "TODO eliminate this fn"
  (typecase val
    (null #f)
    (sql-literal (cl-rdbms::value-of val))
    (t #t)))

(defun sql-length (str)
  (sql-function-call :name "length" :arguments (list str)))

;;;----------------------------------------------------------------------------
;;; Aggregate functions
(defvar *aggregate-functions* (make-hash-table)
  "Map from lisp function symbol to the corresponing SQL aggregate function.")

(defstruct aggregate-function
  (initial-state nil :type function)
  (accumulate nil :type function)
  (extract nil :type function))

(defun aggregate-function-name-p (function-name)
  (gethash function-name *aggregate-functions*))

(defun aggregate-function-for (function-name)
  (gethash function-name *aggregate-functions*))

(defmacro define-aggregate-function (function-name &rest args)
  `(progn
    (setf (gethash ',function-name *aggregate-functions*)
     (make-aggregate-function ,@args))))

(define-aggregate-function count
    :initial-state (constantly 0)
    :accumulate (lambda (val state) (if val (1+ state) state))
    :extract #'identity)

(define-aggregate-function min
    :initial-state (constantly nil)
    :accumulate (lambda (val state) (if (or (null state) (and val (lessp val state))) val state))
    :extract #'identity)

(define-aggregate-function max
    :initial-state (constantly nil)
    :accumulate (lambda (val state) (if (or (null state) (and val (greaterp val state))) val state))
    :extract #'identity)

(define-aggregate-function sum
    :initial-state (constantly nil)
    :accumulate (lambda (val state) (if val (if state (+ state val) val) state))
    :extract #'identity)

(define-aggregate-function avg
    :initial-state (constantly (cons 0 0))
    :accumulate (lambda (val state) (if val
                                        (cons (1+ (car state))
                                              (+ (cdr state) val))
                                        state))
    :extract (lambda (state) (if (> (car state) 0)
                                 (/ (cdr state) (car state))
                                 nil)))

;;;----------------------------------------------------------------------------
;;; Literals
;;;
(defun sql-null-literal-p (sql)
  (or (null sql)
      (and (typep sql 'sql-literal)
           (or (eq (rdbms::value-of sql) :null)
               (and (eq (rdbms::value-of sql) nil)
                    (or (null (cl-rdbms::type-of sql))
                        (not (typep (cl-rdbms::type-of sql) 'cl-rdbms::sql-boolean-type))))))))

(defun sql-literal-p (sql)
  (typep sql 'cl-rdbms::sql-literal*))

(defun sql-null-literal ()
  (sql-literal :value :null))

(defun sql-false-literal-p (sql)
  (and (typep sql 'sql-literal)
       (eq (rdbms::value-of sql) #f)
       (typep (rdbms::type-of sql) 'rdbms::sql-boolean-type)))

(defun sql-false-literal ()
  (sql-literal :value #f :type (make-instance 'cl-rdbms::sql-boolean-type)))

(defun sql-true-literal-p (sql)
  (and (typep sql 'sql-literal)
       (eq (rdbms::value-of sql) #t)
       (typep (rdbms::type-of sql) 'rdbms::sql-boolean-type)))

(defun sql-true-literal ()
  (sql-literal :value #t :type (make-instance 'cl-rdbms::sql-boolean-type)))

;;;----------------------------------------------------------------------------
;;; Helpers
;;;
(defun ensure-class-and-subclasses-exported (class)
  (ensure-exported class)
  (mapc 'ensure-exported (persistent-effective-sub-classes-of class)))