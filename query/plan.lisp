(in-package :cl-perec)

;;;;
;;;; The executable query represented by a plan tree.
;;;; Lisp code that executes the query is generated from the plan.
;;;;


(defclass* plan-node ()
  ((query)))

(defclass* list-result-node (plan-node)
  ((list :type list))
  (:documentation "Creates a result-set from a constant list."))

(defclass* sql-query-node (plan-node)
  ((result-type)
   (distinct nil)
   (columns)
   (tables)
   (where nil)
   (order-by nil))
  (:documentation "Creates a result-set from the records returned by an SQL query."))

(defclass* unary-operation-node (plan-node)
  ((input :type plan-node)
   (bindings :type function :documentation "Function that creates a list of bindings
 when called with the name of the input record."))
  (:documentation "Base class for transformer nodes with one input."))

(defclass* filter-operation (unary-operation-node)
  ((condition :documentation "A boolean expression."))
  (:documentation "Filters the result-set by a boolean condition."))

(defclass* projection-operation (unary-operation-node)
  ((values :type list :documentation "List of expressions."))
  (:documentation "Computes a function of the input record."))

(defclass* sort-operation (unary-operation-node)
  ((sort-spec :type list :documentation "List of :asc/:desc and expression pairs.")))

(defclass* unique-operation (unary-operation-node)
  ()
  (:documentation "Make the records of the result-set unique."))

(defclass* group-operation (unary-operation-node)
  ((group-by)
   (exprs))
  (:documentation "Groups records and computes aggregate functions."))

(defclass* conversion-operation (unary-operation-node)
  ((result-type)
   (flatp))
  (:documentation "Converts the result-set to the expected result type."))

(defclass* delete-operation (unary-operation-node)
  ((variables)))


;;; TODO: union,intersection,difference,product,join

;;;
;;; Generate initial plan for the query.
;;;

(defgeneric generate-plan (query)
  (:documentation "Generates a PLAN for the QUERY.")

  (:method ((query query))

           (case (action-of query)
             (:collect
                 (if (contradictory-p query)
                     (add-conversion
                      (generate-list-result-set nil query)
                      query)
                     (add-conversion
                      (add-unique
                       (add-projection
                        (add-sorter
                         (add-filter
                          (generate-sql-query query)
                          query)
                         query)
                        query)
                       query)
                      query)))
             (:purge
                 (if (contradictory-p query)
                     nil
                     (add-lisp-delete
                      (add-filter
                       (generate-sql-query query)
                       query)
                      query))))))

(defun contradictory-p (query)
  (is-false-literal
   (simplify-boolean-syntax
    (make-macro-call :macro 'and :args (asserts-of query)))))

(defun generate-list-result-set (list query)
  (make-instance 'list-result-node
                 :query query
                 :list list))

(defun generate-sql-query (query)
  (bind ((tables (sql-table-references-for query)))
    (if tables
        (bind ((instance (make-instance 'sql-query-node
                                        :query query
                                        :result-type (result-type-of query)
                                        :distinct (uniquep query)
                                        :columns (sql-select-list-for query)
                                        :tables  tables)))
          (dolist (variable (query-variables-of query))
            (when (joined-variable-p variable)
              (add-sql-where-conditions
               instance
               (list (sql-join-condition-for-joined-variable variable)))))
          instance)
        (generate-list-result-set nil query))))

(defun add-filter (input query)
  (bind ((asserts (asserts-of query)))
    (if asserts
       (make-instance 'filter-operation
                      :query query
                      :input input
                      :bindings (query-variable-bindings query)
                      :condition `(and ,@asserts))
       input)))

(defun add-sorter (input query)
  (bind ((order-by (order-by-of query)))
    (if (and order-by (or (member :asc order-by) (member :desc order-by)))
        (make-instance 'sort-operation
                       :query query
                       :input input
                       :bindings (query-variable-bindings query)
                       :sort-spec order-by)
        input)))

(defun add-projection (input query)
  (bind ((collects (collects-of query))
         (aggregates (collect-aggregate-calls collects)))
    (if aggregates
        (bind ((field-names (mapcar 'field-name-for aggregates))
               (substitution (mapcar #'cons aggregates field-names))
               (new-collects (substitute-syntax collects substitution)))
          (make-instance 'projection-operation
                         :query query
                         :bindings (field-bindings field-names)
                         :input (make-instance 'group-operation
                                               :query query
                                               :input input
                                               :bindings (query-variable-bindings query)
                                               :group-by nil
                                               :exprs aggregates)
                         :values new-collects))
        (make-instance 'projection-operation
                       :query query
                       :bindings (query-variable-bindings query)
                       :input input
                       :values (collects-of query)))))

(defun add-unique (input query)
  (if (uniquep query)
      (make-instance 'unique-operation
                     :query query
                     :input input)
      input))

(defun add-conversion (input query)
  (make-instance 'conversion-operation
                 :query query
                 :input input
                 :result-type (result-type-of query)
                 :flatp (flatp query)))

(defun add-lisp-delete (input query)
  (bind ((bindings (query-variable-bindings query))
         (variables (action-args-of query)))
    (make-instance 'delete-operation
                   :query query
                   :bindings bindings
                   :variables variables
                   :input input)))

;;;
;;; Optimize the plan
;;;

(defgeneric optimize-plan (plan)

  (:method (plan-node)
           plan-node)
  
  (:method ((op unary-operation-node))
           (setf (input-of op) (optimize-plan (input-of op)))
           op)

  (:method ((delete delete-operation))
           (let ((*suppress-alias-names* #t)) ;FIXME
             (call-next-method)))

  (:method ((filter filter-operation))
           (call-next-method)
           (if (typep (input-of filter) 'sql-query-node)
               (bind ((sql-query (input-of filter))
                      ((values sql-conditions lisp-conditions) (to-sql (rest (condition-of filter)))))
                 (add-sql-where-conditions sql-query sql-conditions)
                 (if lisp-conditions
                     (progn
                       (setf (condition-of filter) `(and ,@lisp-conditions))
                       filter)
                     sql-query))
               filter))

  (:method ((sort sort-operation))
           (call-next-method)
           (if (typep (input-of sort) 'sql-query-node)
               (bind ((sql-query (input-of sort))
                      (sql-order-by (order-by-to-sql (sort-spec-of sort))))
                 (if sql-order-by
                     (progn
                       (setf (order-by-of sql-query) sql-order-by)
                       sql-query)
                     sort))
               sort))

  (:method ((grouping group-operation))
           (call-next-method))

  (:method ((projection projection-operation))
           (call-next-method)))

(defun order-by-to-sql (order-by)
  (iter (for (dir expr) on order-by by 'cddr)
        (bind (((values sort-key success) (transform-to-sql expr))
               (ordering (ecase dir (:asc :ascending) (:desc :descending))))
          (if success
              (collect `(sql-sort-spec :sort-key ,sort-key :ordering ,ordering))
              (leave)))))

(defun to-sql (list)
  (iter (for form in list)
        (bind (((values sql success) (transform-to-sql form)))
          (if success
              (collect sql into sql-forms)
              (collect form into lisp-forms)))
        (finally (return-from to-sql (values sql-forms lisp-forms)))))

;;;
;;; Compile plan to lisp code.
;;;

(defgeneric compile-plan (plan)
  (:documentation "Compiles a PLAN to executable lisp code.")

  (:method ((list-node list-result-node))
           (with-slots (list) list-node
             `(make-list-result-set ',list)))

  (:method ((sql-query sql-query-node))
           (with-slots (query result-type distinct columns tables where order-by) sql-query
             `(open-result-set
               ',result-type
               ,(partial-eval
                 `(sql-select
                   :distinct ,distinct
                   :columns (list ,@columns)
                   :tables  (list ,@tables)
                   :where ,where
                   :order-by (list ,@order-by))
                 query)
               ,@(when (eq result-type 'scroll)
                       (list (partial-eval
                              `(sql-select
                                :columns (list (cl-rdbms::sql-count-*))
                                :tables (list (sql-table-reference-for
                                               (sql-subquery
                                                :query
                                                (sql-select
                                                 :distinct ,distinct
                                                 :columns (list ,@columns)
                                                 :tables (list ,@tables)
                                                 :where ,where))
                                               'records)))
                              query))))))

  (:method ((filter filter-operation))
           (with-slots (input bindings condition) filter
             (with-unique-names (row)
               `(make-filtered-result-set
                 ,(compile-plan input)
                 (lambda (,row)
                   (let (,@(funcall bindings row :referenced-by condition))
                     ,condition))))))

  (:method ((projection projection-operation))
           (with-slots (input bindings values) projection
             (with-unique-names (row)
               `(make-mapped-result-set
                 ,(compile-plan input)
                 (lambda (,row)
                   (let (,@(funcall bindings row :referenced-by values))
                     ;;,(ignorable-variables-declaration variables)
                     (list ,@values)))))))

  (:method ((sort sort-operation))
           (with-slots (input bindings sort-spec) sort
             (with-unique-names (row1 row2)
               (flet ((rename-variables (expr suffix)
                        (bind ((variables (collect-query-variables expr))
                               (subs (mapcar #L(cons !1 (concatenate-symbol (name-of !1) suffix))
                                             variables)))
                          (substitute-syntax expr subs))))
                 (bind ((bindings1 (funcall bindings row1 :suffix "1" :referenced-by sort-spec))
                        (bindings2 (funcall bindings row2 :suffix "2" :referenced-by sort-spec))
                        (sort-spec1 (rename-variables (copy-query sort-spec) "1"))
                        (sort-spec2 (rename-variables (copy-query sort-spec) "2")))
                   `(make-ordered-result-set
                     ,(compile-plan input)
                     (lambda (,row1 ,row2)
                       (let (,@bindings1 ,@bindings2)
                         ,(generate-comparator sort-spec1 sort-spec2)))))))))

  (:method ((delta unique-operation))
           (with-slots (input) delta
             `(make-unique-result-set
               ,(compile-plan input))))

  (:method ((gamma group-operation))
           (with-slots (input bindings group-by exprs) gamma
             (with-unique-names (row acc)
               `(make-grouped-result-set
                 ,(compile-plan input)
                 (lambda (,row)
                   (declare (ignorable ,row))
                   (let (,@(funcall bindings row :referenced-by group-by))
                     (list ,@group-by)))
                 (lambda ()
                   ,(aggregate-init-fn-body-for exprs))
                 (lambda (,row ,acc)
                   (let (,@(funcall bindings row :referenced-by exprs))
                     ,(aggregate-collect-fn-body-for acc exprs)))
                 (lambda (,acc)
                   ,(aggregate-map-fn-body-for acc exprs))))))

  (:method ((conversion conversion-operation))
           (with-slots (input result-type flatp) conversion
             (ecase result-type
               (list `(to-list ,(compile-plan input) :flatp ,flatp))
               (scroll `(to-scroll ,(compile-plan input) :flatp ,flatp)))))

  (:method ((delete delete-operation))
           (with-slots (input bindings variables) delete
             (etypecase input
               ;;
               ;; There are list filter conditions:
               ;;   execute the filter+delete by iterating on the objects to be deleted
               ;;   use execute :visitor because it's implemented with cursors
               (filter-operation
                (bind ((input2 (input-of input))
                       (condition (condition-of input2)))
                  (assert (typep input2 'sql-query-node))
                  (with-unique-names (row)
                    `(execute ,(compile-plan input2)
                      :visitor
                      (lambda (,row)
                        (let ,(funcall bindings row :referenced-by (append variables condition))
                          (when ,condition
                            ,@(mapcar
                               (lambda (variable) `(make-transient ,variable))
                               variables))))))))
               ;; sql deletes
               (sql-query-node
                (assert (length=1 variables)) ; FIXME
                (bind ((variable (first (variables-of delete)))
                       (type (xtype-of variable))
                       (tables (when (persistent-class-p type) (tables-for-delete type))))
                  (if (length=1 tables) ; simple delete
                      `(execute ,(sql-delete-from-table (first tables) :where (where-of input)))
                      (bind ((temp-table (rdbms-name-for 'deleted-ids :table))
                             (select-deleted-ids `(sql-select
                                                   :columns (list
                                                             ,(sql-id-column-reference-for
                                                               variable))
                                                   :tables  (list
                                                             ,@(tables-of input))
                                                   :where ,(where-of input)))
                             (create-table (create-temporary-table temp-table
                                                                   (list
                                                                    (sql-column
                                                                     :name +oid-id-column-name+
                                                                     :type +oid-id-sql-type+))
                                                                   select-deleted-ids
                                                                   *database*))
                             (delete-where `(sql-subquery
                                             :query
                                             (sql-select
                                              :columns (list ',+oid-id-column-name+)
                                              :tables (list ',temp-table))))
                             (deletes (delete nil
                                              (mapcar
                                               (lambda (table)
                                                 (sql-delete-for-subselect table delete-where))
                                               tables)))
                             (drop-table (drop-temporary-table temp-table *database*)))
                        `(execute-protected
                          ,create-table
                          (list ,@deletes)
                          ,drop-table)))))))))

(defgeneric create-temporary-table (table-name columns subselect database)
  (:method (table-name columns subselect database)
           `(sql-create-table :name ',table-name
                              :temporary #t
                              :columns (list ,@columns)
                              :as (sql-subquery :query ,subselect)))

  (:method (table-name columns subselect (database oracle))
           (ensure-oracle-temporary-table-exists table-name columns)
           `(sql-insert
             :table ',table-name
             :columns (list ,@columns)
             :subselect (sql-subquery
                         :query ,subselect))))

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
           `(sql-drop-table :name ',table-name))

  (:method (table-name (database oracle))
           nil))

;;;
;;; Helpers
;;;

(defun field-name-for (expr)
  (if (and (consp expr) (aggregate-function-name-p (first expr)))
      (gensym (symbol-name (first expr)))
      (gensym "AGGR")))

(defun field-bindings (field-names)
  (lambda (row &key suffix referenced-by)
    (declare (ignore referenced-by))
    (flet ((name-for (field)
             (if suffix (concatenate-symbol field suffix) field)))
      (iter (for field in field-names)
           (for i from 0)
           (collect `(,(name-for field) (elt ,row ,i)))))))

(defun query-variable-bindings (query)
  (bind ((variables (query-variables-of query))
         (prefetchp (prefetchp query)))
    (lambda (row &key suffix referenced-by)
      (bind ((referenced-variables (collect-query-variables referenced-by)))
        (flet ((name-for (variable)
                (if suffix
                    (concatenate-symbol (name-of variable) suffix)
                    (name-of variable))))
         (iter (for variable in variables)
               (for slots = (when prefetchp (prefetched-slots-for variable)))
               (for column-count = (reduce '+ slots
                                           :key 'column-count-of
                                           :initial-value +oid-column-count+))
               (for i initially 0 then (+ i column-count))
               (if (member variable referenced-variables)
                   (collect `(,(name-for variable)
                              (cache-instance-with-prefetched-slots ,row ,i ',slots))))))))))

(defun ignorable-variables-declaration (variables)
  `(declare (ignorable ,@(mapcar 'name-of variables))))

(defun generate-comparator (sort-spec-1 sort-spec-2)
  (bind ((lessp (ecase (first sort-spec-1) (:asc 'lessp) (:desc 'greaterp)))
         (expr1 (second sort-spec-1))
         (expr2 (second sort-spec-2)))
    (if (null (cddr sort-spec-1))       ; last
        `(,lessp ,expr1 ,expr2)
        (with-unique-names (obj1 obj2)
          `(let ((,obj1 ,expr1)
                 (,obj2 ,expr2))
            (or
             (,lessp ,obj1 ,obj2)
             (and
              (equal ,obj1 ,obj2)
              ,(generate-comparator (cddr sort-spec-1) (cddr sort-spec-2)))))))))

(defun check-aggregate-calls (exprs)
  "Returns true if some expression contains a call to an aggregate function.
If true then all query variables must be under some aggregate call."
  (some 'aggregate-expression-p exprs))

(defun collect-aggregate-calls (expr)
  "Returns the list of aggregate function calls in EXPR.
If the result is not NIL, then each query variable must be aggregated,
 otherwise an error signaled."
  (bind ((result (syntax-fold
                  expr
                  (lambda (node)
                    (cons (query-variable-p node)
                          (and (function-call-p node)
                               (aggregate-function-name-p (fn-of node))
                               (list node))))
                  (lambda (parent &rest children)
                    (if (cdr parent)
                        (cons #f (cdr parent))
                        (cons (or (car parent) (some #'car children))
                              (reduce #'union (mapcar #'cdr children) :initial-value nil)))))))
    (when (and (car result) (cdr result))
      (error "Aggregate expression expected, found: ~S" expr)) ;; TODO should be checked earlier
    (cdr result)))

(defun collect-query-variables (syntax)
  (syntax-fold
   syntax
   (lambda (node)
     (when (query-variable-p node)
       (list node)))
   (lambda (parent &rest children)
     (declare (ignore parent))
     (if (length=1 children)
         (first children)
         (reduce #'union children :initial-value nil)))))

(defun aggregate-init-fn-body-for (exprs)
  `'(,@(mapcar
        (lambda (expr)
          (funcall (aggregate-function-initial-state (aggregate-function-for (fn-of expr)))))
        exprs)))

(defun aggregate-collect-fn-body-for (acc-var exprs)
  `(list
    ,@(mapcar
       (lambda (expr)
         `(funcall
           ,(aggregate-function-accumulate (aggregate-function-for (fn-of expr)))
           ,(first (args-of expr)) (pop ,acc-var)))
       exprs)))

(defun aggregate-map-fn-body-for (acc-var exprs)
  `(vector
    ,@(mapcar
       (lambda (expr)
         `(funcall
           ,(aggregate-function-extract (aggregate-function-for (fn-of expr)))
           (pop ,acc-var)))
       exprs)))

(defun add-sql-where-conditions (sql-query conditions)
  (when conditions
    (bind ((where (where-of sql-query)))
      (if (null where)
          (progn
            (setf (where-of sql-query) `(sql-and ,@conditions)))
          (progn
            (assert (and (consp where) (eq (first where) 'sql-and)))
            (setf (where-of sql-query) (nconc where conditions)))))))

