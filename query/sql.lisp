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

(defun sql-select-for-query (query)
  "Emits code for the QUERY."
  (ecase (action-of query)
    (:collect
        `(sql-select
          :distinct ,(uniquep query)
          :columns (list ,@(sql-select-list-for query))
          :tables  (list ,@(sql-table-references-for query))
          :where ,(where-clause-of query)
          :order-by (list ,@(order-by-of query))))
    (:purge
     (bind ((variable (first (action-args-of query))))
       (assert variable)
       `(sql-select
         :columns (list ,(sql-id-column-reference-for variable))
         :tables (list ,@(sql-table-references-for query))
         :where ,(where-clause-of query))))))

(defun sql-select-oids-for-entity (entity-spec)
  (bind ((tables (rest (primary-tables-of (find-class entity-spec)))))
    (sql-select-oids-from-tables tables 'sql-union)))

(defun sql-select-oids-from-tables (tables set-operation)
  "Generates a select for the union or intersection of oids from TABLES."
  (declare (type (member sql-union sql-intersect) set-operation))
  (case (length tables)
    (0 nil)
    (1 (sql-select-oids-from-table (first tables)))
    (otherwise (apply set-operation (mapcar 'sql-select-oids-from-table tables)))))

(defgeneric sql-select-oids-from-table (table)
  (:method ((table table))
           (sql-select :columns +oid-column-names+ :tables (list (name-of table))))
  (:method ((table sql-table-alias))
           (sql-select :columns +oid-column-names+ :tables (list table))))

;;;----------------------------------------------------------------------------
;;; Deletes
(defun sql-deletes-for-query (query)
  "Returns two values, the first is a list of SQL statements that deletes the records, the second is a cleanup form."
  (bind ((variable (first (action-args-of query)))
         (type (xtype-of variable))
         (tables (when (persistent-class-p type) (tables-for-delete type))))
    (if (simple-purge-p query)
      (bind ((table (first tables)))
        `(list ,(sql-delete-from-table table :where (where-clause-of query))))
      (bind ((temp-table (rdbms-name-for 'deleted-ids))
             (create-table `(sql-create-table
                             :name ',temp-table
                             :temporary #t
                             :columns (list (sql-identifier :name ',+id-column-name+))
                             :as (sql-subquery :query ,(sql-select-for-query query))))
             (delete-where `(sql-subquery
                             :query
                             (sql-select
                              :columns (list ',+id-column-name+)
                              :tables (list ',temp-table))))
             (deletes (mapcar #L(sql-delete-for-subselect !1 delete-where) tables))
             (drop-table `(sql-drop-table
                           :name ',temp-table)))
        (values `(list ,create-table ,@deletes)
                drop-table)))))

(defun simple-purge-p (query)
  "The purge is simple if it deletes records from one table only and no need to join other tables for the condition."
  (and (typep query 'simple-query)
       (eq (action-of query) :purge)
       (length=1 (query-variables-of query))
       (persistent-class-p (xtype-of (first (query-variables-of query))))
       (length=1 (tables-for-delete (xtype-of (first (query-variables-of query)))))))

(defun sql-delete-for-subselect (table subselect)
  (sql-delete-from-table
   table
   :where `(sql-in
            ,(sql-id-column-reference-for table)
            ,subselect)))

(defun sql-delete-from-table (table &key where)
  `(sql-delete
    :table ,(sql-table-reference-for table nil)
    :where ,where))

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
;;; Alias names

(defvar *suppress-alias-names* nil)

(defgeneric sql-alias-for (element)
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
           ;; select oids and prefetched properties
           ;; TODO: if no prefetch, select the expressions in the collect clause
           (mapcan
            (lambda (variable) (sql-columns-for-variable variable (prefetchp query)))
            (query-variables-of query))))

(defun sql-columns-for-variable (variable prefetchp)
  (bind ((table-alias (sql-alias-for variable)))
    (if prefetchp
        (nconc
         (sql-oid-column-references-for variable)
         (mapcan #L(sql-column-references-for !1 table-alias)
                 (prefetched-properties-for variable)))
        (sql-oid-column-references-for variable))))

(defun prefetched-properties-for (variable)
  (unless (slot-boundp variable 'prefetched-properties)
    (bind ((type (xtype-of variable)))
      (setf (prefetched-properties-of variable)
            (when (persistent-class-p type)
              (collect-if #L(prefetched-property-p type !1)
                          (persistent-effective-slots-of type))))))
  (prefetched-properties-of variable))

(defun prefetched-property-p (class slot)
  (and (prefetched-p slot)
       (eq (table-of slot) (primary-table-of class))))

;;;----------------------------------------------------------------------------
;;; Table references

(defgeneric sql-table-references-for (element)
  (:method ((query query))
           (bind ((variables (query-variables-of query)))
             (mapcar 'sql-table-reference-for variables variables))))

(defgeneric sql-table-reference-for (element alias)
  (:method ((table table) (alias symbol))
           (sql-table-alias :name (name-of table) :alias alias))

  (:method ((subquery sql-subquery) (alias symbol))
           (sql-derived-table :subquery subquery :alias alias))

  (:method ((class persistent-class) (alias symbol))
           (sql-table-reference-for-type class alias))

  (:method ((class-name symbol) (alias symbol))
           (aif (find-class class-name)
                (sql-table-reference-for it alias)
                (error "No persistent class named '~A~%" class-name)))

  (:method ((variable query-variable) (alias symbol))
           (assert (xtype-of variable))
           (sql-table-reference-for-type (xtype-of variable) alias))

  (:method ((syntax syntax-object) (alias symbol))
           (make-function-call :fn 'sql-table-reference-for :args (list syntax alias)))

  (:method (element (variable query-variable))
           (sql-table-reference-for element (sql-alias-for variable)))

  (:method (element (entity persistent-class))
           (sql-table-reference-for element (sql-alias-for entity)))

  (:method (element (syntax syntax-object))
           (make-function-call :fn 'sql-table-reference-for :args (list element syntax))))

(defgeneric sql-table-reference-for-type (type &optional alias)
  
  (:method ((class persistent-class) &optional alias)
           (bind ((tables (rest (primary-tables-of class)))) ; TODO handle UNION/APPEND
             (case (length tables)
               (0 (error "No primary table for persistent class: ~A" class))
               (1 (sql-table-reference-for (first tables) alias))
               (t (sql-table-reference-for
                   (sql-subquery :query (sql-select-oids-from-tables tables 'sql-union))
                   alias)))))

  (:method ((class-name symbol) &optional alias)
           (sql-table-reference-for-type (find-class class-name) alias))

  (:method ((type syntax-object) &optional alias) ;; type unknown at compile time
           (make-function-call
            :fn 'sql-table-reference-for-type
            :args (list (backquote-type-syntax type) `',alias)))

  (:method ((combined-type list) &optional alias)
           (flet ((ensure-sql-query (table-ref)
                    (assert (not (syntax-object-p table-ref)))
                    (etypecase table-ref
                      (sql-table-alias (sql-subquery :query (sql-select-oids-from-table table-ref)))
                      (sql-derived-table (cl-rdbms::subquery-of table-ref))))) ; TODO export
             (bind ((operands (mapcar 'sql-table-reference-for-type (rest combined-type))))
               (if (some 'syntax-object-p operands) ;; some type unknown at compile time
                   (make-function-call
                    :fn 'sql-table-reference-for-type
                    :args (list (backquote-type-syntax combined-type) `',alias))
                   (ecase (car combined-type)
                     (or (sql-table-reference-for
                          (sql-subquery
                           :query (apply 'sql-union (mapcar #L(ensure-sql-query !1) operands)))
                          alias))
                     (and (sql-table-reference-for
                           (sql-subquery
                            :query (apply 'sql-intersect (mapcar #L(ensure-sql-query !1) operands)))
                           alias))
                     (not (not-yet-implemented))))))))

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
           (sql-column-reference-for (first (columns-of slot)) qualifier))

  (:method (element qualifier)
           (sql-column-reference-for element (sql-alias-for qualifier))))

(defun sql-id-column-reference-for (qualifier)
  (sql-column-reference-for +id-column-name+ qualifier))

(defgeneric sql-column-references-for (element qualifier)
  (:method ((column-names list) qualifier)
           (mapcar #L(sql-column-reference-for !1 qualifier) column-names))

  (:method ((association-end persistent-association-end-slot-definition) qualifier)
           (sql-column-references-for (columns-of association-end) qualifier))

  (:method ((slot persistent-slot-definition) qualifier)
           (sql-column-references-for (columns-of slot) qualifier)))

(defun sql-oid-column-references-for (qualifier)
  (sql-column-references-for +oid-column-names+ qualifier))


;;;----------------------------------------------------------------------------
;;; Subselects

(defun sql-exists-subselect-for-variable (variable type)
  "Returns an sql expression which evaluates to true iff the query variable named VARIABLE-NAME
 has the type TYPE."
  `(sql-exists
    (sql-subquery
     :query
     (sql-select
      :columns (list 1)
      :tables (list ,(sql-table-reference-for-type type type))
      :where ,(sql-join-condition-for variable type nil)))))

(defun sql-exists-subselect-for-association-end (variable association-end)
  "Returns an sql expression which evaluates to true iff the query variable VARIABLE
 has associated objects through ASSOCIATION-END."
  (bind ((class (slot-definition-class association-end)))
    `(sql-exists
      (sql-subquery
       :query
       (sql-select
        :columns (list 1)
        :tables (list ,(sql-table-reference-for class (sql-alias-for class)))
        :where ,(sql-join-condition-for variable class association-end))))))

(defun sql-aggregate-subselect-for-variable (aggregate-function n-association-end 1-var)
  (bind ((1-association-end (other-association-end-of n-association-end))
         (n-entity (slot-definition-class 1-association-end))
         (n-var (make-query-variable :name (gensym (symbol-name (class-name n-entity)))
                                     :xtype n-entity)))
    `(sql-subquery
      :query
      (sql-select
       :columns (list (,aggregate-function ,(sql-id-column-reference-for n-var)))
       :tables (list ,(sql-table-reference-for n-var n-var))
       :where ,(sql-join-condition-for 1-var n-var 1-association-end)))))

(defun sql-aggregate-subselect-for-m-n-association-end (aggregate-function association-end variable)
  (bind ((other-end (other-association-end-of association-end))
         (table (primary-table-of (association-of association-end))))
    `(sql-subquery
      :query
      (sql-select
       :columns (list (,aggregate-function ,(sql-column-reference-for association-end table)))
       :tables (list (sql-identifier :name ',(name-of table)))
       :where (sql-=
               ,(sql-column-reference-for other-end table)
               ,(sql-id-column-reference-for variable))))))

(defun sql-subselect-for-m-n-association (association-end variable)
  (bind ((other-end (other-association-end-of association-end))
         (table (primary-table-of (association-of association-end))))
    `(sql-subquery
      :query
      (sql-select
       :columns (list ,(sql-column-reference-for association-end table))
       :tables (list (sql-identifier :name ',(name-of table)))
       :where (sql-=
               ,(sql-column-reference-for other-end table)
               ,(sql-id-column-reference-for variable))))))



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
    `(sql-exists
      (sql-subquery
       :query
       (sql-select
        :columns (list 1)
        :tables (list (sql-identifier :name ',(name-of table)))
        :where ,(sql-join-condition-for object-1 object-2 association-end-2))))))


;;;----------------------------------------------------------------------------
;;; Operators

(defvar *sql-operators* (make-hash-table)
  "Map from lisp function names to the corresponding SQL operator.")

(defun sql-operator-p (operator)
  (gethash operator *sql-operators*))

(defun sql-operator-for (operator)
  (gethash operator *sql-operators*))

(defun define-sql-operator (lisp-operator sql-operator)
  (setf (gethash lisp-operator *sql-operators*) sql-operator))

(defun define-chained-sql-operator (lisp-operator sql-operator)
  (define-sql-operator lisp-operator
      (lambda (first-arg &rest more-args)
        (if (null more-args)
            #t
            (apply 'sql-and (iter (for arg in more-args)
                                  (for prev-arg previous arg initially first-arg)
                                  (collect (funcall sql-operator prev-arg arg))))))))

(defun define-pairwise-sql-operator (lisp-operator sql-operator)
  (define-sql-operator lisp-operator
      (lambda (first-arg &rest more-args)
        (if (null more-args)
            #t
            (apply 'sql-and
                   (iter outer (for rest-args on more-args)
                         (for first previous (car rest-args) initially first-arg)
                         (iter (for second in rest-args)
                               (in outer (collect (funcall sql-operator first second))))))))))

(define-sql-operator 'eq 'sql-=)
(define-sql-operator 'eql 'sql-=)
(define-sql-operator 'equal 'sql-=)
(define-chained-sql-operator '= 'sql-=)
(define-sql-operator 'string= 'sql-=)

(define-sql-operator 'and 'sql-and)
(define-sql-operator 'or 'sql-or)
(define-sql-operator 'not 'sql-not)

(define-chained-sql-operator '> 'sql->)
(define-chained-sql-operator '< 'sql-<)
(define-chained-sql-operator '>= 'sql->=)
(define-chained-sql-operator '<= 'sql-<=)
(define-pairwise-sql-operator '/= 'sql-<>)

(define-sql-operator '+ 'sql-+)
(define-sql-operator '- 'sql--)
(define-sql-operator '* 'sql-*)
(define-sql-operator '/ 'sql-/)
(define-sql-operator 'mod 'sql-%)
(define-sql-operator 'expt 'sql-^)
(define-sql-operator 'sqrt 'sql-\|/)
(define-sql-operator 'abs 'sql-@)

(define-sql-operator 'subseq 'sql-subseq)
(define-sql-operator 'strcat 'sql-\|\|)

(define-sql-operator 'like 'sql-like)
(define-sql-operator 'scan 'sql-regex-match)

(define-sql-operator 'null 'sql-is-null)

(defun sql-subseq (seq start &optional end)
  "TODO: other sequnce types"
  (if (or (not (numberp start)) (> start 0) end)
      (sql-substring seq (sql-+ start 1) (sql-- (sql-length seq) start))
      seq))

(defun sql-substring (str start length)
  (sql-function-call :name "substring" :arguments (list str start length)))

(defun sql-regex-match (regex target &key (start 0) end)
  "TODO: this works for PostgreSQL only"
  (sql-~ (sql-subseq target start end)  regex))

(defun sql-length (str)
  (sql-function-call :name "char_length" :arguments (list str)))

;;;----------------------------------------------------------------------------
;;; Aggregate functions

(defvar *aggregate-functions* (make-hash-table)
  "Map from lisp function symbol to the corresponing SQL aggregate function.")

(defun sql-aggregate-function-name-p (function-name)
  (gethash function-name *aggregate-functions*))

(defun sql-aggregate-function-for (function-name)
  (gethash function-name *aggregate-functions*))

(defun define-aggregate-function (lisp-function-name sql-function-name)
  (setf (gethash lisp-function-name *aggregate-functions*) sql-function-name))

(define-aggregate-function 'length 'sql-count)
(define-aggregate-function 'min 'sql-min)
(define-aggregate-function 'max 'sql-max)
(define-aggregate-function 'sum 'sql-sum)
(define-aggregate-function 'avg 'sql-avg)
