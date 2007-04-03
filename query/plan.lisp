(in-package :cl-perec)

;;;;
;;;; The executable query represented by a plan tree.
;;;; Lisp code that executes the query is generated from the plan.
;;;;


(defclass* plan-node ()
  ())

(defclass* list-result-node (plan-node)
  ((list :type list))
  (:documentation "Creates a result-set from a constant list."))

(defclass* sql-query-node (plan-node)
  ((result-type)
   (sql-query))
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

;;; TODO: union,intersection,difference,product,join

;;;
;;;
;;;

(defgeneric generate-plan (query)
  (:documentation "Generates a PLAN for the QUERY.")

  (:method ((query simple-query))

           (if (contradictory-p query)
               (add-conversion
                (generate-list-result-set nil)
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
                query))))

(defun contradictory-p (query)
  (is-false-literal
   (simplify-boolean-syntax
    (make-macro-call :macro 'and :args (asserts-of query)))))

(defun generate-list-result-set (list)
  (make-instance 'list-result-node
                 :list list))

(defun generate-sql-query (query)
  (make-instance 'sql-query-node
                 :result-type (result-type-of query)
                 :sql-query (partial-eval (sql-select-for-query query) query)))

(defun add-filter (input query)
  (bind ((asserts (asserts-of query)))
    (if asserts
       (make-instance 'filter-operation
                      :input input
                      :bindings (query-variable-bindings query asserts)
                      :condition `(and ,@asserts))
       input)))

(defun add-sorter (input query)
  (bind ((order-by (order-by-of query)))
    (if (and order-by (or (member :asc order-by) (member :desc order-by)))
        (make-instance 'sort-operation
                       :input input
                       :bindings (query-variable-bindings query order-by)
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
                        :bindings (field-bindings field-names)
                        :input (make-instance 'group-operation
                                              :input input
                                              :bindings (query-variable-bindings query collects)
                                              :group-by nil
                                              :exprs aggregates)
                        :values new-collects))
        (make-instance 'projection-operation
                       :bindings (query-variable-bindings query collects)
                       :input input
                       :values (collects-of query)))))

(defun add-unique (input query)
  (if (uniquep query)
      (make-instance 'unique-operation
                     :input input)
      input))

(defun add-conversion (input query)
  (make-instance 'conversion-operation
                 :input input
                 :result-type (result-type-of query)
                 :flatp (flatp query)))


;;;
;;; Compile plan to lisp code.
;;;

(defgeneric compile-plan (plan)
  (:documentation "Compiles a PLAN to executable lisp code.")

  (:method ((list-node list-result-node))
           (with-slots (list) list-node
             `(make-list-result-set ',list)))

  (:method ((sql-query sql-query-node))
           (with-slots (result-type sql-query) sql-query
             `(open-result-set ',result-type ,sql-query)))

  (:method ((filter filter-operation))
           (with-slots (input bindings condition) filter
             (with-unique-names (row)
               `(make-filtered-result-set
                 ,(compile-plan input)
                 (lambda (,row)
                   (let (,@(funcall bindings row))
                     ;;,(ignorable-variables-declaration variables)
                     ,condition))))))

  (:method ((projection projection-operation))
           (with-slots (input bindings values) projection
             (with-unique-names (row)
               `(make-mapped-result-set
                 ,(compile-plan input)
                 (lambda (,row)
                   (let (,@(funcall bindings row))
                     ;;,(ignorable-variables-declaration variables)
                     (list ,@values)))))))

  (:method ((sort sort-operation))
           (with-slots (input bindings sort-spec) sort
             (with-unique-names (row1 row2)
               `(make-ordered-result-set
                 ,(compile-plan input)
                 (lambda (,row1 ,row2)
                   (let (,@(funcall bindings row1 "1")
                           ,@(funcall bindings row2 "2"))
                     ,(generate-comparator sort-spec)))))))

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
                   (let (,@(funcall bindings row))
                     (list ,@group-by)))
                 (lambda (,row ,acc)
                   (let (,@(funcall bindings row))
                     ,(accumulator-fn-body-for exprs)))
                 (lambda (,acc)
                   ,(aggregate-value-fn-body-for exprs))))))

  (:method ((conversion conversion-operation))
           (with-slots (input result-type flatp) conversion
             (ecase result-type
               (list `(to-list ,(compile-plan input) ,flatp))
               (scroll `(to-scroll ,(compile-plan input)))))))

;;;
;;; Helpers
;;;

(defun field-name-for (expr)
  (if (and (consp expr) (aggregate-function-name-p (first expr)))
      (gensym (symbol-name (first expr)))
      (gensym "AGGR")))

(defun field-bindings (field-names)
  (lambda (row &optional suffix)
    (flet ((name-for (field)
             (if suffix (concatenate-symbol field suffix) field)))
      (iter (for field in field-names)
           (for i from 0)
           (collect `(,(name-for field) (nth ,i ,row)))))))

(defun query-variable-bindings (query exprs)
  (bind ((variables (query-variables-of query))
         (referenced-variables (collect-query-variables exprs))
         (prefetchp (prefetchp query)))
    (lambda (row &optional suffix)
      (flet ((name-for (variable)
               (if suffix
                   (concatenate-symbol (name-of variable) suffix)
                   (name-of variable))))
        (iter (for variable in variables)
              (for slots = (when prefetchp (prefetched-slots-for variable)))
              (for column-count = (reduce '+ slots
                                          :key 'column-count-of
                                          :initial-value (length +oid-column-names+)))
              (for i initially 0 then (+ i column-count))
              (if (member variable referenced-variables)
                  (collect `(,(name-for variable)
                             (cache-object-with-prefetched-slots ,row ,i ',slots)))))))))

(defun ignorable-variables-declaration (variables)
  `(declare (ignorable ,@(mapcar 'name-of variables))))

(defun generate-comparator (variables sort-spec)
  (labels ((rename-variables (expr suffix)
             "Adds the SUFFIX to each query variable symbol in EXPR."
             (sublis (mapcar #L(cons (name-of !1) (concatenate-symbol (name-of !1) suffix))
                             variables)
                     expr)))
    (bind ((lessp (ecase (first sort-spec) (:asc 'lessp) (:desc 'greaterp)))
           (expr1 (rename-variables (second sort-spec) "1"))
           (expr2 (rename-variables (second sort-spec) "2")))
      (if (null (cddr sort-spec)) ; last
          `(,lessp ,expr1 ,expr2)
          (with-unique-names (obj1 obj2)
            `(let ((,obj1 ,expr1)
                   (,obj2 ,expr2))
              (or
               (,lessp ,obj1 ,obj2)
               (and
                (equal ,obj1 ,obj2)
                ,(generate-comparator variables (cddr sort-spec))))))))))

(defun check-aggregate-calls (exprs)
  "Returns true if some expression contains a call to an aggregate function.
If true then all query variables must be under some aggregate call."
  (some 'aggregate-expression-p exprs))

(defun aggregate-function-name-p (name)
  (member name '(sum avg))) ; TODO

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

(defun accumulator-fn-body-for (exprs)
  (declare (ignore exprs))
  nil) ;TODO

(defun aggregate-value-fn-body-for (exprs)
  (declare (ignore exprs))
  nil) ; TODO

