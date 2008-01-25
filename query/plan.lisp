(in-package :cl-perec)

;;;;
;;;; The executable query represented by a plan tree.
;;;; Lisp code that executes the query is generated from the plan.
;;;;


(defclass* plan-node ()
  ((query)
   (binder :type function :documentation "Function that creates a list of bindings
 when called with the name of the input record.")))

(defclass* list-result-node (plan-node)
  ((list :type list))
  (:documentation "Creates a result-set from a constant list."))

(defclass* sql-query-node (plan-node)
  ((result-type)
   (distinct nil)
   (columns)
   (tables)
   (where nil)
   (group-by nil)
   (having nil)
   (order-by nil))
  (:documentation "Creates a result-set from the records returned by an SQL query."))

(defclass* unary-operation-node (plan-node)
  ((input :type plan-node))
  (:documentation "Base class for transformer nodes with one input."))

(defclass* filter-operation (unary-operation-node)
  ((condition :documentation "A boolean expression."))
  (:documentation "Filters the result-set by a boolean condition."))

(defclass* projection-operation (unary-operation-node)
  ((values :type list :documentation "List of expressions."))
  (:documentation "Computes a function of the input record."))

(defclass* sort-operation (unary-operation-node)
  ((sort-spec :type list :documentation "List of :ascending/:descending and expression pairs.")))

(defclass* unique-operation (unary-operation-node)
  ()
  (:documentation "Make the records of the result-set unique."))

(defclass* group-operation (unary-operation-node)
  ((group-by :documentation "Grouping expressions.")
   (collected-expressions :documentation "Collected expressions, each expression depends on the grouping expressions."))
  (:documentation "Groups records and computes aggregate functions."))

(defclass* conversion-operation (unary-operation-node)
  ((result-type)
   (flatp))
  (:documentation "Converts the result-set to the expected result type."))

(defclass* delete-operation (unary-operation-node)
  ((variables)))

;;; TODO: union,intersection,difference,product,join

(defmethod print-object ((node plan-node) stream)
  (princ (class-name (class-of node)) stream)
  (pprint-newline :mandatory stream))

(defmethod print-object ((node unary-operation-node) stream)
  (call-next-method)
  (pprint-indent :current 2 stream)
  (print-object (input-of node) stream))

;;;
;;; Generate initial plan for the query.
;;;

(defgeneric generate-plan (query)
  (:documentation "Generates a PLAN for the QUERY.")

  (:method :around ((query query))
           (aprog1 (call-next-method)
             (qlog.dribble "Initial plan:~%~<~S~:>" it)))

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
                         (add-having-filter
                          (add-grouping
                           (add-where-filter
                            (generate-sql-query query)
                            query)
                           query)
                          query)
                         query)
                        query)
                       query)
                      query)))
             (:purge
                 (if (contradictory-p query)
                     nil
                     (add-lisp-delete
                      (add-where-filter
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
                 :binder (query-variable-binder query)
                 :list list))

(defun generate-sql-query (query)
  (bind ((tables (sql-table-references-for query)))
    (if tables
        (bind ((instance (make-instance 'sql-query-node
                                        :query query
                                        :binder (query-variable-binder query)
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

(defun add-where-filter (input query)
  (bind ((asserts (asserts-of query)))
    (if asserts
       (make-instance 'filter-operation
                      :query query
                      :binder (binder-of input)
                      :input input
                      :condition `(and ,@asserts))
       input)))

(defun add-grouping (input query)
  (bind ((group-by (group-by-of query))
         (collects (collects-of query))
         ((values aggregates non-aggregated-variables)
          (collect-aggregate-calls collects group-by))) ;; FIXME order-by,having
    (when (and aggregates non-aggregated-variables)
      (error "Collect clause (~S) not compatible with the group-by clause (~S)"
             collects group-by))
    (if (or group-by aggregates)
        (make-instance 'group-operation
                       :query query
                       :binder (field-binder aggregates) ;; TODO expressions above this node
                       :input input                         ;; (in sort/projection)
                       :group-by group-by                   ;; could refer only these variables
                       :collected-expressions aggregates)
        input)))

(defun add-having-filter (input query)
  (bind ((having (having-of query)))
    (if having
       (make-instance 'filter-operation
                      :query query
                      :binder (binder-of input)
                      :input input
                      :condition `(and ,@having))
       input)))

(defun add-sorter (input query)
  (bind ((order-by (order-by-of query)))
    (if (and order-by (or (member :asc order-by)
                          (member :ascending order-by)
                          (member :desc order-by)
                          (member :descending order-by))) ;; FIXME ???
        (progn
          (make-instance 'sort-operation
                         :query query
                         :binder (binder-of input)
                         :input input
                         :sort-spec order-by))
        input)))

(defun add-projection (input query)
  (make-instance 'projection-operation
                 :query query
                 :binder nil ;; no bindings needed above the projection
                 :input input
                 :values (collects-of query)))

(defun add-unique (input query)
  (if (uniquep query)
      (make-instance 'unique-operation
                     :query query
                     :binder (binder-of input)
                     :input input)
      input))

(defun add-conversion (input query)
  (make-instance 'conversion-operation
                 :query query
                 :binder nil
                 :input input
                 :result-type (result-type-of query)
                 :flatp (flatp query)))

(defun add-lisp-delete (input query)
  (make-instance 'delete-operation
                 :query query
                 :binder nil
                 :variables (action-args-of query)
                 :input input))

;;;
;;; Optimize the plan
;;;

(defun optimize-plan (plan)
  (aprog1 (%optimize-plan plan)
    (qlog.dribble "Improved plan:~%~<~S~:>" it)))

(defgeneric %optimize-plan (plan)

  (:method (plan-node)
           plan-node)
  
  (:method ((op unary-operation-node))
           (setf (input-of op) (%optimize-plan (input-of op)))
           op)

  (:method ((delete delete-operation))
           (let ((*suppress-alias-names* #t)) ;FIXME
             (call-next-method)))

  (:method ((filter filter-operation))
           (call-next-method)
           (typecase (input-of filter)
               (sql-query-node
                ;; Move filter conditions that can be mapped to sql into the
                ;; sql select.
                (bind ((sql-query (input-of filter))
                       ((values sql-conditions lisp-conditions)
                        (to-sql (rest (condition-of filter)))))
                  (if (null (group-by-of sql-query))
                      (add-sql-where-conditions sql-query sql-conditions)
                      (add-sql-having-conditions sql-query sql-conditions))
                  (if lisp-conditions
                      (progn
                        (setf (condition-of filter) `(and ,@lisp-conditions))
                        filter)
                      sql-query)))
               (t
                filter)))

  (:method ((sort sort-operation))
           (call-next-method)
           (typecase (input-of sort)
               (sql-query-node
                (bind ((sql-query (input-of sort))
                       (sql-order-by (order-by-to-sql (sort-spec-of sort))))
                  (if sql-order-by
                      (progn
                        (setf (order-by-of sql-query) sql-order-by)
                        sql-query)
                      sort)))
               (t
                sort)))

  (:method ((grouping group-operation))
           (call-next-method)
           (typecase (input-of grouping)
               (sql-query-node
                ;; If we are grouping the result of an sql select,
                ;; and each grouping expression is a column reference and
                ;; each collected expression can be mapped to sql,
                ;; then merge the grouping with the sql select.
                (bind ((sql-query (input-of grouping))
                       ((values sql-exprs lisp-exprs) (to-sql (collected-expressions-of grouping)))
                       ((values sql-groupby lisp-groupby) (to-sql (group-by-of grouping))))
                  (if (or lisp-exprs lisp-groupby) ;; TODO check each group-by is a column-ref
                      grouping
                      (progn
                        (setf (binder-of sql-query) (binder-of grouping)
                              (columns-of sql-query) sql-exprs
                              (group-by-of sql-query) sql-groupby)
                        sql-query))))
               (t
                grouping)))

  (:method ((projection projection-operation))
           (call-next-method)
           (typecase (input-of projection)
             (sql-query-node
              ;; If no persistent object returned by the query, and the collects
              ;; can be mapped to sql, then merge the projection with the sql select.
              (bind ((sql-query (input-of projection))
                     (collects (collects-of (query-of projection)))
                     (persistent-object-query-p (some (lambda (expr)
                                                        (bind ((type (persistent-type-of expr)))
                                                          (and (not (sql-text-p expr))
                                                               (or (eql type +unknown-type+)
                                                                   (contains-syntax-p type)
                                                                   (persistent-class-p type)
                                                                   (set-type-p* type)))))
                                                      collects))
                     ((values sql-exprs lisp-exprs) (to-sql collects)))
                (if (or lisp-exprs persistent-object-query-p) ;; TODO refine condition
                    projection                                ;; all needed table is joined?
                    (progn
                      (setf (binder-of sql-query) (field-binder collects) ;;(binder-of projection)
                            (columns-of sql-query) sql-exprs)
                      projection))))
             (t
              projection))))

(defun order-by-to-sql (order-by)
  (iter (for (dir expr) on order-by by 'cddr)
        (bind (((values sort-key success) (transform-to-sql expr))
               (ordering (ecase dir
                           ((:asc :ascending) :ascending)
                           ((:desc :descending) :descending))))
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

(defun sql-text-p (expr)
  (and (macro-call-p expr)
       (eq (macro-of expr) 'sql-text)))


;;;
;;; Compile plan to lisp code.
;;;

(defun compile-plan (plan)
  (aprog1 (%compile-plan plan)
    (qlog.dribble "Compiled plan:~%~S" it)))


(defgeneric %compile-plan (plan)
  (:documentation "Compiles a PLAN to executable lisp code.")

  (:method ((list-node list-result-node))
    (with-slots (list) list-node
      `(make-list-result-set ',list)))

  (:method ((sql-query sql-query-node))
    (with-slots (query result-type distinct columns tables where group-by having order-by)
        sql-query
      `(open-result-set
        ',result-type
        ,(partial-eval
          `(sql-select
             :distinct ,distinct
             :columns (list ,@columns)
             :tables  (list ,@tables)
             :where ,where
             :group-by (list ,@group-by)
             :having ,having
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
                                             :where ,where
                                             :group-by (list ,@group-by)
                                             :having ,having
                                             ))
                                         'records)))
                       query))))))

  (:method ((filter filter-operation))
    (with-slots (input condition) filter
      (with-unique-names (row)
        (bind (((values bindings condition) (funcall (binder-of input) row condition)))
          `(make-filtered-result-set
            ,(%compile-plan input)
            (lambda (,row)
              (let (,@bindings)
                ,condition)))))))

  (:method ((projection projection-operation))
    (with-slots (input values) projection
      (with-unique-names (row)
        (bind (((values bindings values) (funcall (binder-of input) row values)))
          `(make-mapped-result-set
            ,(%compile-plan input)
            (lambda (,row)
              (let (,@bindings)
                ;;,(ignorable-variables-declaration variables)
                (list ,@values))))))))

  (:method ((sort sort-operation))
    (with-slots (input sort-spec) sort
      (with-unique-names (row1 row2)
        (flet ((rename-variables (expr suffix)
                 (bind ((variables (collect-query-variables expr))
                        (subs (mapcar #L(cons !1 (concatenate-symbol (name-of !1) suffix))
                                      variables)))
                   (substitute-syntax expr subs))))
          (bind ((binder (binder-of input))
                 (bindings1 (funcall binder row1 sort-spec :suffix "1"))
                 (bindings2 (funcall binder row2 sort-spec :suffix "2"))
                 (sort-spec1 (rename-variables (copy-query sort-spec) "1")) ;; FIXME
                 (sort-spec2 (rename-variables (copy-query sort-spec) "2")))
            `(make-ordered-result-set
              ,(%compile-plan input)
              (lambda (,row1 ,row2)
                (let (,@bindings1 ,@bindings2)
                  ,(generate-comparator sort-spec1 sort-spec2)))))))))

  (:method ((delta unique-operation))
    (with-slots (input) delta
      `(make-unique-result-set
        ,(%compile-plan input))))

  (:method ((gamma group-operation))
    (with-slots (input group-by collected-expressions) gamma
      (with-unique-names (row acc)
        (bind ((binder (binder-of input))
               ((values group-by-bindings group-by) (funcall binder row group-by))
               ((values collect-bindings collected-expressions) (funcall binder row collected-expressions)))
          `(make-grouped-result-set
            ,(%compile-plan input)
            (lambda (,row)
              (declare (ignorable ,row))
              (let (,@group-by-bindings)
                (list ,@group-by))) ;; FIXME compared with equal, not ok for local-time
            (lambda ()
              ,(aggregate-init-fn-body-for collected-expressions))
            (lambda (,row ,acc)
              (let (,@collect-bindings)
                ,(aggregate-collect-fn-body-for acc collected-expressions)))
            (lambda (,acc)
              ,(aggregate-map-fn-body-for acc collected-expressions)))))))

  (:method ((conversion conversion-operation))
    (with-slots (input result-type flatp) conversion
      (ecase result-type
        (list `(to-list ,(%compile-plan input) :flatp ,flatp))
        (scroll `(to-scroll ,(%compile-plan input) :flatp ,flatp)))))

  (:method ((delete delete-operation))
    (with-slots (input variables) delete
      (etypecase input
        ;;
        ;; There are list filter conditions:
        ;;   execute the filter+delete by iterating on the objects to be deleted
        ;;   use execute :visitor because it's implemented with cursors
        (filter-operation
         (bind ((input2 (input-of input))
                (condition (condition-of input)))
           (assert (typep input2 'sql-query-node))
           (with-unique-names (row)
             (bind ((bindings (funcall (binder-of input) row (append variables condition))))
               `(execute ,(%compile-plan input2)
                         :visitor
                         (lambda (,row)
                           (let ,bindings
                             (when ,condition
                               ,@(mapcar
                                  (lambda (variable) `(make-transient ,variable))
                                  variables)))))))))
        ;; sql deletes
        (sql-query-node
         (assert (length=1 variables))  ; FIXME
         (bind ((variable (first (variables-of delete)))
                (type (persistent-type-of variable))
                (tables (when (persistent-class-p type) (tables-for-delete type))))
           (if (length=1 tables)        ; simple delete
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
           `(sql-create-table
             :name ',table-name
             :temporary #t
             :columns (list ,@(mapcar (lambda (column)
                                        (sql-identifier :name (slot-value column 'arnesi:name)))
                                      columns))
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

(defun field-binder (exprs)
  (bind ((names (mapcar (lambda (expr)
                          (if (and (function-call-p expr)
                                   (aggregate-function-name-p (fn-of expr)))
                              (gensym (symbol-name (fn-of expr)))
                              (gensym "VAR")))
                        exprs))
         (substitutions (mapcar #'cons exprs names)))
    (lambda (row referenced-by &key suffix (start-index 0))
      (values
       (iter (for field in names)
             (for expr in exprs)
             (for i from start-index)
             (for variable = (if suffix (concatenate-symbol field suffix) field))
             (for type = (persistent-type-of expr))
             (collect
                 `(,variable
                   ,(cond
                     ((query-variable-p expr)
                      `(object-reader ,row ,i))
                     ((and (slot-access-p expr)
                           (not (contains-syntax-p type))
                           (not (eq type +unknown-type+)))
                      `(funcall ',(compute-column-reader (persistent-type-of expr)) ,row ,i))
                     ((and (slot-access-p expr) (not (eq type +unknown-type+)))
                      `(funcall (compute-column-reader ,(backquote-type-syntax (persistent-type-of expr))) ,row ,i))
                     (t
                      `(if (eq (elt ,row ,i) :null) nil (elt ,row ,i))))))) ;; FIXME call reader
       (substitute-syntax referenced-by substitutions) ;; FIXME mutating
       (+ start-index (length names))))))

(defcfun (compute-column-reader :memoize-test-fn equalp :computed-in compute-as) (type)
  (bind ((reader (compute-reader type)))
    (if (or (null-subtype-p type) (unbound-subtype-p type))
        (lambda (row index)
          (bind ((value (elt row index)))
            (if (eq value :null)
                nil
                (funcall reader row index))))
        reader)))

(defun query-variable-binder (query)
  (query-variable-binder2 (query-variables-of query)
                          (prefetch-mode-of query)))

(defun query-variable-binder2 (variables &optional (prefetch-mode :accessed))
  (lambda (row referenced-by &key suffix (start-index 0))
      (bind ((referenced-variables (collect-query-variables referenced-by))
             (end-index start-index))
        (flet ((name-for (variable)
                (if suffix
                    (concatenate-symbol (name-of variable) suffix)
                    (name-of variable))))
          (values
           (iter (for variable in variables)
                 (for slots = (prefetched-slots-for variable prefetch-mode))
                 (for column-count = (reduce '+ slots
                                             :key 'column-count-of
                                             :initial-value +oid-column-count+))
                 (for i initially 0 then (+ i column-count))
                 (incf end-index column-count)
                 (if (member variable referenced-variables)
                     (collect `(,(name-for variable)
                                (cache-instance-with-prefetched-slots ,row ,i ,(persistent-type-of variable) ',slots
                                 ',(mapcar #'column-count-of slots))))))
           referenced-by
           end-index)))))

(defun binder-append (binder1 binder2)
  (lambda (row referenced-by &key suffix)
    (bind (((values bindings1 expr1 end-index1)
            (funcall binder1 row referenced-by :suffix suffix :start-index 0))
           ((values bindings2 expr2 end-index2)
            (funcall binder2 row expr1 :suffix suffix :start-index end-index1)))
      (values
       (append bindings1 bindings2)
       expr2
       end-index2))))

(defun ignorable-variables-declaration (variables)
  `(declare (ignorable ,@(mapcar 'name-of variables))))

(defun generate-comparator (sort-spec-1 sort-spec-2)
  (bind ((lessp (ecase (first sort-spec-1)
                  ((:asc :ascending) 'lessp)
                  ((:desc :descending) 'greaterp)))
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

(defun collect-aggregate-calls (expr group-by)
  "Returns the list of aggregate function calls in EXPR and the list of non-aggregated
 query variables."
  (bind ((result (syntax-fold
                  expr
                  (lambda (node)
                    (cons (when (and (query-variable-p node)
                                     (not (member node group-by :test 'syntax=)))
                            (list node))
                          (when (or (and (function-call-p node)
                                         (aggregate-function-name-p (fn-of node)))
                                    (member node group-by :test 'syntax=))
                            (list node))))
                  (lambda (parent &rest children)
                    (if (cdr parent)
                        (cons nil (cdr parent))
                        (cons (reduce #'union (mapcar #'car children) :initial-value (car parent))
                              (reduce #'union (mapcar #'cdr children) :initial-value nil)))))))
    (values (cdr result) (car result))))

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

(defun aggregate-function-call-p (expr)
  (and (function-call-p expr)
       (aggregate-function-name-p (fn-of expr))))

(defun aggregate-init-fn-body-for (exprs)
  `'(,@(mapcar
        (lambda (expr)
          (when (aggregate-function-call-p expr)
              (funcall (aggregate-function-initial-state (aggregate-function-for (fn-of expr))))))
        exprs)))

(defun aggregate-collect-fn-body-for (acc-var exprs)
  `(list
    ,@(mapcar
       (lambda (expr)
         (if (aggregate-function-call-p expr)
             `(funcall
               ,(aggregate-function-accumulate (aggregate-function-for (fn-of expr)))
               ,(first (args-of expr)) (pop ,acc-var))
             `(progn
               (pop ,acc-var)
               ,expr)))
       exprs)))

(defun aggregate-map-fn-body-for (acc-var exprs)
  `(vector
    ,@(mapcar
       (lambda (expr)
         (if (aggregate-function-call-p expr)
             `(funcall
               ,(aggregate-function-extract (aggregate-function-for (fn-of expr)))
               (pop ,acc-var))
             `(pop ,acc-var)))
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

(defun add-sql-having-conditions (sql-query conditions)
  (when conditions
    (bind ((having (having-of sql-query)))
      (if (null having)
          (progn
            (setf (having-of sql-query) `(sql-and ,@conditions)))
          (progn
            (assert (and (consp having) (eq (first having) 'sql-and)))
            (setf (having-of sql-query) (nconc having conditions)))))))