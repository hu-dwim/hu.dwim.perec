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
  (bind ((tables (rest (primary-tables-of (find-class class-name))))) ; TODO: APPEND/UNION
    (sql-select-oids-from-tables tables 'sql-union)))

(defun sql-select-oids-from-tables (tables set-operation)
  "Generates a select for the union or intersection of oids from TABLES."
  (declare (type (member sql-union sql-intersect) set-operation))
  (case (length tables)
    (0 nil)
    (1 (sql-select-oids-from-table (first tables)))
    (otherwise (apply set-operation (mapcar 'sql-select-oids-from-table tables)))))

(defgeneric sql-select-oids-from-table (table)
  (:documentation "Generates a select for the oids in TABLE.")
  (:method ((table table))
           (sql-select :columns +oid-column-names+ :tables (list (name-of table))))
  (:method ((table sql-table-alias))
           (sql-select :columns +oid-column-names+ :tables (list table))))

;;;----------------------------------------------------------------------------
;;; Deletes

(defun sql-delete-for-subselect (table subselect)
  "Generate a delete command for records in TABLE whose oid is in the set returned by SUBSELECT."
  (sql-delete-from-table
   table
   :where `(sql-in
            ,(sql-id-column-reference-for table)
            ,subselect)))

(defun sql-delete-from-table (table &key where)
  "Generate a delete command for records in TABLE that satisfies the WHERE clause."
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
                 (prefetched-slots-for variable)))
        (sql-oid-column-references-for variable))))

(defun prefetched-slots-for (variable)
  (bind ((type (xtype-of variable)))
    (when (persistent-class-p type)
      (collect-if #L(eq (table-of !1) (primary-table-of type))
                  (prefetched-slots-of type)))))

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
           (assert (not (eq (xtype-of variable) +unknown-type+)))
           (sql-table-reference-for-type (xtype-of variable) alias))

  (:method ((syntax syntax-object) (alias symbol))
           (make-function-call :fn 'sql-table-reference-for :args (list syntax alias)))

  (:method (element (variable query-variable))
           (sql-table-reference-for element (sql-alias-for variable)))

  (:method (element (class persistent-class))
           (sql-table-reference-for element (sql-alias-for class)))

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

  (:method ((type-name symbol) &optional alias)
           (bind ((class (find-class type-name #f)))
             (typecase class
               (persistent-class (sql-table-reference-for-type class alias))
               (otherwise nil))))

  (:method ((type syntax-object) &optional alias) ;; type unknown at compile time
           (make-function-call
            :fn 'sql-table-reference-for-type
            :args (list (backquote-type-syntax type) `',alias)))

  (:method ((combined-type list) &optional alias)
           (if (contains-syntax-p combined-type)
               ;; delay evaluation until run-time
               (make-function-call
                :fn 'sql-table-reference-for-type
                :args (list (backquote-type-syntax combined-type) `',alias))
               ;; and/or/not types
               (labels ((ensure-sql-query (table-ref)
                          (assert (not (syntax-object-p table-ref)))
                          (etypecase table-ref
                            (sql-table-alias (sql-subquery :query (sql-select-oids-from-table table-ref)))
                            (sql-derived-table (cl-rdbms::subquery-of table-ref)))) ; TODO missing export
                        (ensure-alias (table-ref)
                          (when (or (typep table-ref 'sql-table-alias)
                                    (typep table-ref 'sql-derived-table))
                            (setf (cl-rdbms::alias-of table-ref) alias))
                          table-ref)
                        (combine-types (sql-set-operation types)
                          (bind ((operands (delete nil
                                                   (mapcar 'sql-table-reference-for-type types))))
                           (case (length operands)
                             (0 nil)
                             (1 (ensure-alias (first operands)))
                             (t (sql-table-reference-for
                                 (sql-subquery
                                  :query (apply sql-set-operation
                                                (mapcar #'ensure-sql-query operands)))
                                 alias))))))
                 (case (car combined-type)
                   (or (combine-types 'sql-union (rest combined-type)))
                   (and (combine-types 'sql-intersect (rest combined-type)))
                   (not (not-yet-implemented))
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
           (assert (columns-of slot))
           (sql-column-reference-for (last1 (columns-of slot)) qualifier))

  (:method (element qualifier)
           (sql-column-reference-for element (sql-alias-for qualifier))))

(defun sql-id-column-reference-for (qualifier)
  (sql-column-reference-for +id-column-name+ qualifier))

(defun sql-bound-column-reference-for (slot qualifier)
  (sql-column-reference-for (bound-column-of slot) qualifier))

(defgeneric sql-column-references-for (element qualifier)
  (:method ((column-names list) qualifier)
           (mapcar #L(sql-column-reference-for !1 qualifier) column-names))

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
  (bind ((class (slot-definition-class (other-association-end-of association-end))))
    `(sql-exists
      (sql-subquery
       :query
       (sql-select
        :columns (list 1)
        :tables (list ,(sql-table-reference-for class (sql-alias-for class)))
        :where ,(sql-join-condition-for variable class (other-association-end-of association-end)))))))

(defun sql-aggregate-subselect-for-variable (aggregate-function n-association-end 1-var)
  (bind ((1-association-end (other-association-end-of n-association-end))
         (n-class (slot-definition-class 1-association-end))
         (n-var (make-query-variable :name (gensym (symbol-name (class-name n-class)))
                                     :xtype n-class)))
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

(defun sql-subselect-for-secondary-association-end (association-end variable)
  (bind ((primary-association-end (other-association-end-of association-end))
         (class (slot-definition-class primary-association-end)))
    `(sql-subquery
      :query
      (sql-select
       :columns (list ,(sql-id-column-reference-for nil))
       :tables (list ,(sql-table-reference-for class nil))
       :where (sql-=
               ,(sql-column-reference-for primary-association-end nil)
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
;;; Boundp check
;;;
(defgeneric sql-slot-boundp (variable slot)

  (:method ((variable query-variable) (slot persistent-effective-slot-definition))
           (bind ((slot-type (slot-definition-type slot)))
             (cond
               ((complex-type-p slot-type)
                (sql-is-not-null (sql-bound-column-reference-for slot variable)))
               ((unbound-subtype-p slot-type)
                (sql-is-not-null (sql-column-reference-for slot variable)))
               (t ;; TODO: should be handled by partial eval
                (sql-true-literal)))))
  )

(defgeneric sql-slot-is-null (variable slot)

  (:method ((variable query-variable) (slot persistent-effective-slot-definition))
           (bind ((slot-type (slot-definition-type slot)))
             (cond
               ((complex-type-p slot-type)
                (sql-and
                 (sql-is-not-null (sql-bound-column-reference-for slot variable))
                 (sql-is-null (sql-column-reference-for slot variable))))
               ((null-subtype-p slot-type)
                (sql-is-null (sql-column-reference-for slot variable)))
               (t                                ;; TODO: partial eval
                (sql-false-literal))))))

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

(define-sql-operator 'eq 'sql-equal)
(define-sql-operator 'eql 'sql-equal)
(define-sql-operator 'equal 'sql-equal)
(define-sql-operator '= (chained-operator 'sql-= #t))
;; (define-sql-operator 'string= 'sql-string=) ; sql-string= tricky to implement because string=
                                               ; accepts chars and symbols too, use equal instead

(define-sql-operator 'and 'sql-and)
(define-sql-operator 'or 'sql-or)
(define-sql-operator 'not 'sql-not)

(define-sql-operator '> (chained-operator 'sql-> #t))
(define-sql-operator '< (chained-operator 'sql-< #t))
(define-sql-operator '>= (chained-operator 'sql->= #t))
(define-sql-operator '<= (chained-operator 'sql-<= #t))
(define-sql-operator '/= (pairwise-operator 'sql-<> #t))

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

(defun sql-equal (sql-expr-1 sql-expr-2 &key unbound-check-1 unbound-check-2 null-check-1 null-check-2)
  "Generates an equality test for the two sql expression and the corresponding boundness checks.
If one of the values is unbound, the test yields NULL, otherwise it yields true or false (two NULL
value is equal, when they represent the NIL lisp value)."
  (flet ((sql-is-null (sql unbound-check null-check)
           (cond
             ((sql-null-literal-p sql)
              (sql-true-literal))
             ((and unbound-check null-check)
              `(sql-if ,unbound-check
                       (sql-null-literal)
                       ,null-check))
             (unbound-check
              `(sql-if ,unbound-check
                       (sql-null-literal)
                       (sql-false-literal)))
             (null-check
              null-check)
             (t
              (sql-false-literal))))
         (wrap-with-null-check (eq-check)
           (cond
             ((and null-check-1 null-check-2)
              `(sql-or
                (sql-and ,null-check-1 ,null-check-2)
                (sql-and (sql-not ,null-check-1) (sql-not ,null-check-2) ,eq-check)))
             (null-check-1
              `(sql-and (sql-not ,null-check-1) ,eq-check))
             (null-check-2
              `(sql-and (sql-not ,null-check-2) ,eq-check))
             (t
              eq-check)))
         (wrap-with-unbound-check (eq-check)
           (cond
             ((and unbound-check-1 unbound-check-2)
              `(sql-if (sql-or ,unbound-check-1 ,unbound-check-2)
                       (sql-null-literal)
                       ,eq-check))
             (unbound-check-1
              `(sql-if ,unbound-check-1
                       (sql-null-literal)
                       ,eq-check))
             (unbound-check-2
              `(sql-if ,unbound-check-2
                       (sql-null-literal)
                       ,eq-check))
             (t
              eq-check))))
    (cond
      ((sql-null-literal-p sql-expr-1)
       (sql-is-null sql-expr-2 unbound-check-2 null-check-2))
      ((sql-null-literal-p sql-expr-2)
       (sql-is-null sql-expr-1 unbound-check-1 null-check-1))
      (t
       (wrap-with-unbound-check
        (wrap-with-null-check
         `(sql-= ,sql-expr-1 ,sql-expr-2)))))))

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

(defstruct aggregate-function
  (sql)
  (initial-state nil :type function)
  (accumulate nil :type function)
  (extract nil :type function))

(defun aggregate-function-name-p (function-name)
  (gethash function-name *aggregate-functions*))

(defun aggregate-function-for (function-name)
  (gethash function-name *aggregate-functions*))

(defmacro define-aggregate-function (function-name &rest args)
  `(setf (gethash ',function-name *aggregate-functions*)
         (make-aggregate-function ,@args)))

(define-aggregate-function count
    :sql 'sql-count
    :initial-state (constantly 0)
    :accumulate (lambda (val state) (if val (1+ state) state))
    :extract #'identity)

(define-aggregate-function min
    :sql 'sql-min
    :initial-state (constantly nil)
    :accumulate (lambda (val state) (if (or (null state) (and val (lessp val state))) val state))
    :extract #'identity)

(define-aggregate-function max
    :sql 'sql-max
    :initial-state (constantly nil)
    :accumulate (lambda (val state) (if (or (null state) (and val (greaterp val state))) val state))
    :extract #'identity)

(define-aggregate-function sum
    :sql 'sql-sum
    :initial-state (constantly nil)
    :accumulate (lambda (val state) (if val (if state (+ state val) val) state))
    :extract #'identity)

(define-aggregate-function avg
    :sql 'sql-avg
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
           (null (cl-rdbms::value-of sql))
           (or (null (cl-rdbms::type-of sql))
               (not (typep (cl-rdbms::type-of sql) 'cl-rdbms::sql-boolean-type))))))

(defun sql-literal-p (sql)
  (typep sql 'cl-rdbms::sql-literal*))

(defun sql-null-literal ()
  (sql-literal :value nil))

(defun sql-false-literal ()
  (sql-literal :value #f :type (make-instance 'cl-rdbms::sql-boolean-type)))

(defun sql-true-literal ()
  (sql-literal :value #t :type (make-instance 'cl-rdbms::sql-booelan-type)))

