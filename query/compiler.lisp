;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;(declaim-debug)

(enable-pattern-reader #\M)

;;;; TODO: sorting and grouping
;;;; TODO: embedded SQL in queries
;;;; TODO: return nil if there is contradiction between asserts
;;;; TODO: eliminate tautologies from asserts
;;;; TODO: n-m associations
;;;; TODO: delete and update operations
;;;; TODO: recursive selects

;;;
;;; Compile queries
;;;
(defvar *compile-query-counter* 0
  "Number of calls to COMPILE-QUERY. (FOR TESTING)")

(defvar *test-query-compiler* nil
  "When true, the compiled form performs a runtime check by comparing the result of the query
with the result of the naively compiled query.")

(defun reset-compile-query-counter ()
  (setf *compile-query-counter* 0))

(defmethod compile-query :before (query)
  (incf *compile-query-counter*))

(defmethod compile-query ((query query))
  (%compile-query (make-instance 'trivial-query-compiler) query))

(defmethod compile-query ((query simple-query))
  (if *test-query-compiler*
      (%compile-query (make-instance 'debug-query-compiler) query)
      (%compile-query (make-instance 'simple-query-compiler) query)))

(defclass* query-compiler ()
  ()
  (:documentation "Generic query compiler, which can transform to sql any select form."))

(defgeneric %compile-query (compiler query)
  (:documentation "Compiles the query with the specified compiler."))

(defmethod %compile-query ((compiler query-compiler) (query query))
  (unparse-query-syntax
   (optimize-query
    compiler
    (emit-query
     compiler
     (transform-query
      compiler
      query)))))

(defgeneric transform-query (compiler query)
  (:documentation "TODO")

  (:method (compiler query)
           query))

(defgeneric emit-query (compiler query)
  (:documentation "TODO"))

(defgeneric optimize-query (compiler syntax)
  (:documentation "TODO")

  (:method (compiler syntax)
           syntax))

;;;;
;;;; Trivial query compiler
;;;;
(defclass* trivial-query-compiler (query-compiler)
  ()
  (:documentation "Query compiler that can compile any select form, but does not optimize sql queries."))

(defmethod emit-query ((compiler trivial-query-compiler) query)
  (bind ((lexical-variables (lexical-variables-of query))
         (variables (get-query-variable-names query))
         (body (body-of query))
         (body (mapcar 'query-macroexpand body))
         (persistent-object-literals (collect-persistent-object-literals body))
         (persistent-object-variables (mapcar #L(gensym (symbol-name (class-name (class-of !1))))
                                              persistent-object-literals)))
    (with-unique-names (objects result-list)
      `(lambda ,lexical-variables
        (declare (ignorable ,@lexical-variables))
        ,@(mapcar #L(`(load-instance ,!1)) persistent-object-literals)
        (let (,@(mapcar #L(`(,!1 (load-instance ,!2))) persistent-object-variables persistent-object-literals)
              (,objects (mapcar 'cache-object
                                (execute ,(sql-select-oids-for-class 'persistent-object))))
              (,result-list nil))
          (flet ((assert! (cond) (when (not cond) (throw 'fail nil)))
                 (collect (&rest exprs) (push exprs ,result-list))
                 (purge (&rest objects) (mapc 'make-transient objects))
                 (order-by (&rest order-spec) nil)) ; TODO
            (declare (ignorable (function assert!) (function collect) (function purge) (function order-by)))
            (bind-cartesian-product ((,@variables) ,objects)
              (catch 'fail
                (progn
                  ,@(sublis
                     (cons '(assert . assert!)
                           (mapcar 'cons persistent-object-literals persistent-object-variables))
                     body)))))
          ,(add-conversion-to-result-type
            query
            (add-unique-filter
             query
             `(make-list-result-set (nreverse ,result-list)))))))))

(defun add-unique-filter (query form)
  (if (uniquep query)
      `(make-unique-result-set ,form)
      form))

(defun add-conversion-to-result-type (query form)
  (ecase (result-type-of query)
    (list `(to-list ,form ,(flatp query)))
    (scroll `(to-scroll ,form))))

;;;;
;;;; Debug query compiler
;;;;
(defclass* debug-query-compiler (query-compiler)
  ()
  (:documentation "Generic query compiler, which can transform to sql any select form and
wraps the compiled code with a runtime check of the result."))

(defmethod %compile-query ((compiler debug-query-compiler) (query query))
  "Emits code that checks that the result of COMPILED-FORM equals
 to the result of the PREDICATE-FORM."
  (let* ((predicate-form (%compile-query (make-instance 'trivial-query-compiler) query))
         (compiled-form (%compile-query (make-instance 'simple-query-compiler) query))
         (lexical-variables (lexical-variables-of query)))
    (with-unique-names (result expected result-list expected-list)
      (unparse-query-syntax
       `(lambda ,lexical-variables
         (declare (ignorable ,@lexical-variables))
         (bind ((,result (funcall ,compiled-form ,@lexical-variables))
                (,expected (funcall ,predicate-form ,@lexical-variables))
                (,result-list (to-list ,result))
                (,expected-list (to-list ,expected)))
           ;; TODO: set-exclusive-or is not ok for comparing the results, because
           ;;       the result is not a set and (set-exclusive-or '(a b b) '(a a b))
           ;;       returns NIL.
           (when (set-exclusive-or ,result-list ,expected-list :test 'equal)
             (cerror "Return the expected result." 'query-error
                     :query ,query :result ,result-list :expected ,expected-list))
           ,expected))))))

(define-condition query-error ()
  ((query :initarg :query)
   (result :initarg :result)
   (expected :initarg :expected))

  (:report (lambda (condition stream)
             (format stream "Query ~S failed. Result is ~:W, but expected ~:W."
                     (slot-value condition 'query)
                     (slot-value condition 'result)
                     (slot-value condition 'expected))))
  
  (:documentation "Condition signalling that the runtime check of the query failed."))

;;;;---------------------------------------------------------------------------
;;;; Simple query compiler
;;;;
(defclass* simple-query-compiler (query-compiler)
  ()
  (:documentation "Query compiler that can transform to sql to simple select forms."))

(defmethod emit-query ((compiler simple-query-compiler) (query simple-query))
  (ecase (action-of query)
    (:collect (emit-select query))
    (:purge (emit-purge query))))

(defun emit-purge (query)
  (bind ((lexical-variables (lexical-variables-of query))
         (asserts (asserts-of query))
         (variables (query-variables-of query))
         (purge-var (first (action-args-of query)))
         (type (xtype-of purge-var))
         (substitutions (generate-persistent-object-substitutions query)))
    (if asserts
        ;; execute deletes from lisp filter
        (with-unique-names (row)
          `(lambda ,lexical-variables
            (declare (ignorable ,@lexical-variables))
            (let (,@(emit-persistent-object-bindings substitutions))
              ,(substitute-syntax
                `(execute ,(sql-select-for-query query)
                  :visitor
                  (lambda (,row)
                    (let ,(query-variable-bindings variables row #f)
                      ,(ignorable-variables-declaration variables)
                      (when (and ,@asserts)
                        (make-transient ,(first (action-args-of query)))))))
                substitutions))))
        ;; execute deletes from sql
        (bind (((values deletes cleanup) (sql-deletes-for-query query)))
          `(lambda ,lexical-variables
            (declare (ignorable ,@lexical-variables))
            (let (,@(emit-persistent-object-bindings substitutions))
              (invalidate-persistent-flag-of-cached-objects (find-persistent-class* ,type))
              ,(substitute-syntax
                (if cleanup
                    `(unwind-protect (mapc 'execute ,deletes) (execute ,cleanup))
                    `(mapc 'execute ,deletes))
                substitutions)))))))

(defun emit-select (query)
  "Emits code that for the compiled query."
  (bind ((lexical-variables (lexical-variables-of query))
         (substitutions (generate-persistent-object-substitutions query)))
    `(lambda ,lexical-variables
      (declare (ignorable ,@lexical-variables))
      (let (,@(emit-persistent-object-bindings substitutions))
        ,(substitute-syntax
          (compile-plan (generate-plan query))
          substitutions)))))

(defun generate-persistent-object-substitutions (query)
  (bind ((objects (collect-persistent-object-literals query))
         (variables (mapcar #L(gensym (symbol-name (class-name (class-of !1)))) objects)))
    (mapcar 'cons objects variables)))

(defun emit-persistent-object-bindings (substitutions)
  (mapcar #L(`(,(cdr !1) (load-instance ,(car !1)))) substitutions))

;;;;---------------------------------------------------------------------------
;;;; Transformations
;;;;
(defmethod transform-query ((compiler simple-query-compiler) (query simple-query))
  "Transforms the QUERY by pushing down the asserts to the SQL query."
  (macroexpand-query query)
  (parse-query query)
  (normalize-query query)
  (infer-types query)
  (introduce-joined-variables query)
  (partial-eval-asserts query)
  (when (not (contradictory-p query))
    (let ((*suppress-alias-names* (simple-purge-p query)))
      (build-sql query)))
  query)

(defun macroexpand-query (query)
  "Expands query macros in QUERY."
  (setf (asserts-of query)
        (mapcar 'query-macroexpand (asserts-of query))))

(defun parse-query (query)
  (bind ((variables (get-variables query)))
    (setf (asserts-of query) (mapcar #L(parse-query-form !1 variables) (asserts-of query)))
    (setf (action-args-of query) (mapcar #L(parse-query-form !1 variables) (action-args-of query)))
    (setf (order-by-of query) (iter (for (dir expr) on (order-by-of query) by 'cddr)
                                    (nconcing (list dir (parse-query-form expr variables)))))))

(defun normalize-query (query)
    (setf (asserts-of query)
          (mappend #L(conjuncts-of
                      (simplify-boolean-syntax
                       (normalize-syntax
                        (partial-eval !1 query))))
                   (asserts-of query))))

(defun conjuncts-of (syntax)
  "Return a list of the conjuncts in SYNTAX."
  (pattern-case syntax
    (#M(macro-call :macro and) (args-of syntax))
    (#M(literal-value :value #t) nil)
    (?otherwise (list syntax))))

(defgeneric normalize-syntax (syntax)
  (:documentation "Normalizes type asserts to (typep ...) forms to ease further processing:
  (typep <object> '<class-name>)               -> (typep <object> <class>)
  (subtypep (class-of <obj>) '<class-name>) -> (typep <object> <class>)
  (subtypep (class-of <obj>) <type>)         -> (typep <object> <type>)

  if the assoc is 1-1
  (eq (<primary-assoc-end-accessor> <obj1>) <obj2>) -> 
                                           (eq (secondary-assoc-end-accessor <obj2>) <obj1>)")
  
  (:method (syntax)
           syntax)
  (:method ((form compound-form))
           (setf (operands-of form)
                 (mapcar 'normalize-syntax (operands-of form)))
           form)
  (:method ((call function-call))
           (call-next-method)
           (pattern-case call
             (#M(function-call :fn typep
                               :args (?obj #M(literal-value :value (?is ?class persistent-class-p))))
                call)
             (#M(function-call :fn typep
                               :args (?obj #M(literal-value :value (?is ?name persistent-class-name-p))))
                (setf (second (args-of call)) (make-literal-value :value (find-class ?name)))
                call)
             (#M(function-call :fn subtypep
                               :args (#M(function-call :fn class-of :args (?object))
                                        #M(literal-value :value (?is ?name persistent-class-name-p))))
                (make-function-call :fn 'typep
                                    :args (list ?object
                                                (make-literal-value :value (find-class ?name)))))
             (#M(function-call :fn subtypep
                               :args (#M(function-call :fn class-of :args (?object)) ?type))
                (make-function-call :fn 'typep :args (list ?object ?type)))
             ;; TODO reverse 1-1 association-end
             (?otherwise
              call))))

(defun introduce-joined-variables (query)
  (mapc #L(introduce-joined-variables-for !1 query) (asserts-of query))
  (mapc #L(introduce-joined-variables-for !1 query) (action-args-of query))
  (mapc #L(when (syntax-object-p !1) (introduce-joined-variables-for !1 query)) (order-by-of query)))

(defgeneric introduce-joined-variables-for (syntax query)
  (:documentation "Substitutes the arguments of slot accessor forms with joined variables.")
  ;; atoms, unparsed
  (:method (syntax query)
           (values))
  ;; recurse on compound forms
  (:method ((syntax compound-form) query)
           (mapc #L(introduce-joined-variables-for !1 query) (operands-of syntax)))  
  ;; slot access -> ensure that arg is a query variable with the correct type
  (:method ((access slot-access) query)
           (call-next-method)
           (when (association-end-access-p (arg-of access))
             (setf (arg-of access)
                   (joined-variable-for-association-end-access query (arg-of access))))
           (when (slot-of access)
             (setf (arg-of access)
                   (ensure-type query (arg-of access) (slot-definition-class (slot-of access)))))
           (values)))

(defun partial-eval-asserts (query)
  (setf (asserts-of query)
        (mapcar #L(partial-eval !1 query) (asserts-of query))))

(defun build-sql (query)
  "Converts assert conditions and order by specifications to SQL."
  (iter (for variable in (query-variables-of query))
        (when (joined-variable-p variable)
          (add-where-clause query (sql-join-condition-for-joined-variable variable))))
  (setf (asserts-of query)
        (iter (for condition in (asserts-of query))
              (bind (((values sql success) (transform-to-sql condition)))
                (if success
                    (add-where-clause query sql)
                    (collect condition)))))
  (setf (sql-order-by-of query)
        (iter (for (dir expr) on (order-by-of query) by 'cddr)
              (bind (((values sort-key success) (transform-to-sql expr))
                     (ordering (ecase dir (:asc :ascending) (:desc :descending))))
                (if success
                    (collect `(sql-sort-spec :sort-key ,sort-key :ordering ,ordering))
                    (leave)))))
  (when (sql-order-by-of query)
    (setf (order-by-of query) nil)))

;;;----------------------------------------------------------------------------
;;; Optimize
;;;
(defmethod optimize-query ((compiler simple-query-compiler) syntax)
  "Optimize the compiled form."
  ;(simplify-class-references syntax)
  ;(partial-eval syntax)
  syntax)

(defun simplify-class-references (syntax)
  (pattern-case syntax
    (#M(function-call :fn find-class
                      :args (#M(function-call :fn name-of
                                              :args ((? and ?inner #M(function-call :fn class-of
                                                                                    :args (?object)))))))
       (simplify-class-references ?inner))
    (#M(compound-form)
       (setf (operands-of syntax)
             (mapcar 'simplify-class-references (operands-of syntax)))
       syntax)
    (?otherwise
     syntax)))


;;;----------------------------------------------------------------------------
;;; SQL mapping
;;;
(defun transform-to-sql (condition)
  "Transforms the CONDITION of an assert to an SQL expression."
    (bind ((sql nil)
          (success #f))
     (catch 'sql-map-failed
       (setf sql (syntax-to-sql condition))
       (setf success #t))
     (values sql success)))

(defun sql-map-failed ()
  (throw 'sql-map-failed nil))


(defgeneric syntax-to-sql (syntax)
  (:documentation "Maps a lisp form to SQL.")
  
  (:method (syntax)
           (if (free-of-query-variables-p syntax)
               `(value->sql-literal ,syntax ,(backquote-type-syntax (xtype-of syntax)))
               (sql-map-failed)))

  (:method ((literal literal-value))
           (literal-to-sql (value-of literal) (xtype-of literal) literal))

  (:method ((variable lexical-variable))
           `(value->sql-literal ,(name-of variable) ,(backquote-type-syntax (xtype-of variable))))

  (:method ((variable query-variable))
           (sql-id-column-reference-for variable))

  (:method ((access slot-access))
           (slot-access-to-sql (accessor-of access) (arg-of access) access))

  (:method ((call function-call))
           (bind ((fn (fn-of call))
                  (args (args-of call)))
             (function-call-to-sql fn (length args) (first args) (second args) call)))

  (:method ((call macro-call))
           (bind ((macro (macro-of call))
                  (args (args-of call)))
             (macro-call-to-sql macro (length args) (first args) (second args) call))))

(defgeneric literal-to-sql (value type literal)
  (:documentation "Maps a literal value to SQL.")

  (:method (value type literal)
           (cond
             ((keywordp value) value)
             ((syntax-object-p type) `(value->sql-literal ,literal ,(backquote-type-syntax type)))
             (t (value->sql-literal value type)))))

(defgeneric slot-access-to-sql (accessor arg access)
  (:method (accessor arg access)
           (if (free-of-query-variables-p access)
               `(value->sql-literal ,access ,(backquote-type-syntax (xtype-of access)))
               (sql-map-failed)))

  (:method (accessor (variable query-variable) (access slot-access))
           ;; slot accessor
           (bind ((slot (slot-of access)))
             (if (and slot (persistent-slot-p slot))
                 (sql-column-reference-for slot variable)
                 (sql-map-failed))))

  (:method (accessor (variable query-variable) (access association-end-access))
           ;; association-end accessor
           (if (association-end-of access)
               (bind ((association-end (association-end-of access))
                      (association (association-of association-end)))
                 (ecase (association-kind-of association)
                   (:1-1
                    (if (primary-association-end-p association-end)
                        (sql-column-reference-for association-end variable)
                        (sql-subselect-for-secondary-association-end association-end variable)))
                   (:1-n
                    (if (to-one-association-end-p association-end)
                        (sql-column-reference-for association-end variable)
                        (sql-subselect-for-secondary-association-end association-end variable)))
                   (:m-n
                    (sql-subselect-for-m-n-association association-end variable))))
               (sql-map-failed))))


(defgeneric function-call-to-sql (fn n-args arg1 arg2 call)

  (:method (fn n-args arg1 arg2 call)
           (cond
             ;; (<aggregate-fn> (<n-ary-association-end-accessor> <query-var>))
             ;; e.g. (length (messages-of topic)) -->
             ;;         (select count(_m.id) from _message _m where _m.topic_id = _topic.id)
             ((and (sql-aggregate-function-name-p fn) (= n-args 1)
                   (association-end-access-p arg1) (association-end-of arg1)
                   (query-variable-p (arg-of arg1)))
              (ecase (association-kind-of (association-of (association-end-of arg1)))
                (:1-1
                 (sql-map-failed))
                (:1-n
                 (sql-aggregate-subselect-for-variable
                  (sql-aggregate-function-for fn)
                  (association-end-of arg1)
                  (arg-of arg1)))
                (:m-n
                 (sql-aggregate-subselect-for-m-n-association-end
                  (sql-aggregate-function-for fn)
                  (association-end-of arg1)
                  (arg-of arg1)))))
             ;; eq,eql and friends
             ;;   special cases: one or both of the args is NIL
             ;;                  one or both of the args is unbound
             ((member fn '(eq eql equal))
              (sql-equal
               (syntax-to-sql arg1)
               (syntax-to-sql arg2)
               :unbound-check-1 (unbound-check-for arg1)
               :unbound-check-2 (unbound-check-for arg2)
               :null-check-1 (null-check-for arg1)
               :null-check-2 (null-check-for arg2)))
             ;; (<fn> <arg> ...), where <fn> has SQL counterpart
             ;; e.g. (+ 1 2) --> (1 + 2)
             ((sql-operator-p fn)
              `(funcall ',(sql-operator-for fn) ,@(mapcar 'syntax-to-sql (args-of call))))
             ;; When the function call does not depend on query variables
             ;; evaluate it at runtime and insert its value into the SQL query.
             ;; The persistent-objects in the value are converted to object ids.
             ((every 'free-of-query-variables-p (args-of call))
              `(value->sql-literal ,call ,(backquote-type-syntax (xtype-of call))))
             ;; Otherwise the assert containing the function call cannot be mapped to SQL.
             (t
              (sql-map-failed))))

  ;; member form -> in (ignore keyword args, TODO)
  (:method ((fn (eql 'member)) n-args arg1 arg2 call)
           (cond
             ((literal-value-p arg2)
              (if (null (value-of arg2))
                  (sql-false-literal)
                  `(sql-in ,(syntax-to-sql arg1) ,(syntax-to-sql arg2))))
             ((free-of-query-variables-p arg2)
              `(if (null ,(unparse-query-syntax arg2))
                (sql-false-literal)
                ,(unparse-query-syntax `(sql-in ,(syntax-to-sql arg1) ,(syntax-to-sql arg2)))))
             (t `(sql-in ,(syntax-to-sql arg1) ,(syntax-to-sql arg2)))))

  ;; (member <object> (<association-end-accessor> <query-variable>))
  ;; e.g. (member m1 (messages-of topic)) --> (_m1.topic_id = _topic.id)
  (:method ((fn (eql 'member)) (n-args (eql 2)) (object query-variable) (access association-end-access) call)
           ;; member form -> join
           ;;   example:
           ;;   (member m (messages-of t)) -> m.topic_id = t.id
           (if (or (not (query-variable-p (arg-of access)))
                   (not (association-end-of access)))
               (call-next-method)
               (bind ((association-end (association-end-of access))
                      (variable (arg-of access))
                      (association (association-of association-end)))
                 (ecase (association-kind-of association)
                   (:1-1
                    (sql-map-failed))
                   (:1-n
                    (sql-join-condition-for object variable association-end))
                   (:m-n
                    (sql-join-condition-for-m-n-association object variable association-end))))))

  ;; eq form -> join
  ;;   examples:
  ;;   (eq (topic-of message) topic) -> message.topic_id = topic.id
  ;;   (eq (wife-of man) woman)      -> man.wife_id = woman.id
  ;;   (eq (husband-of woman) man)   -> man.wife_id = woman.id
  (:method ((fn (eql 'eq)) (n-args (eql 2)) (access association-end-access) object call)

           (if (not (association-end-of access))
               (sql-map-failed)
               (bind ((association-end (association-end-of access))
                      (other-end (other-association-end-of association-end))
                      (variable (arg-of access))
                      (association (association-of association-end)))
                 (ecase (association-kind-of association)
                   (:1-1
                    (if (primary-association-end-p association-end)
                        (call-next-method)
                        (syntax-to-sql
                         (make-function-call ;; reverse
                          :fn fn
                          :args (list (make-association-end-access :association-end other-end
                                                                   :accessor (reader-name-of other-end)
                                                                   :args (list object))
                                      variable)))))
                   (:1-n
                    (call-next-method))
                   (:m-n
                    (sql-map-failed))))))

  (:method ((fn (eql 'eq)) (n-args (eql 2)) object (access association-end-access) call)
           (function-call-to-sql fn 2 access object call))

  ;; (typep variable type)
  ;;   example:
  ;;   (typep o #<class user>) -> exists(select 1 from user u where u.id = o.id)
  (:method ((fn (eql 'typep)) (n-args (eql 2)) (variable query-variable) (type literal-value) call)
           (if (persistent-class-p (value-of type))
               (sql-exists-subselect-for-variable variable (value-of type))
               (call-next-method)))

  ;; (null (accessor variable))
  (:method ((fn (eql 'null)) (n-args (eql 1)) (access slot-access) arg2 call)
           (bind ((slot (slot-of access)))
             (if (and slot
                      (not (typep slot 'persistent-association-end-effective-slot-definition))
                      (query-variable-p (arg-of access)))
                 (sql-slot-is-null (arg-of access) slot)
                 (call-next-method))))

  ;; (slot-boundp variable slot-name)
  (:method ((fn (eql 'slot-boundp)) (n-args (eql 2)) (variable query-variable) (slot-name literal-value) call)
           ;; TODO: accept non-literal slot-name
           (bind ((slot-name (when (symbolp (value-of slot-name)) (value-of slot-name)))
                  (slot (when slot-name (find-slot-definition variable slot-name))))
             (if (and slot (typep slot 'persistent-effective-slot-definition))
                 (sql-slot-boundp variable slot)
                 (sql-map-failed)))))



(defgeneric macro-call-to-sql (macro n-args arg1 arg2 call)
  (:method (macro n-args arg1 arg2 call)
           (cond
             ((sql-operator-p macro)
              `(funcall ',(sql-operator-for macro) ,@(mapcar 'syntax-to-sql (args-of call))))
             ((every 'free-of-query-variables-p (args-of call))
              `(value->sql-literal ,call ,(backquote-type-syntax (xtype-of call))))
             (t
              (sql-map-failed)))))

(defun free-of-query-variables-p (syntax)
  (typecase syntax
    (query-variable #f)
    (unparsed-form (free-of-query-variables-p (form-of syntax)))
    (compound-form (every 'free-of-query-variables-p (operands-of syntax)))
    (cons (and (free-of-query-variables-p (car syntax))
               (free-of-query-variables-p (cdr syntax))))
    (otherwise #t)))

(defun find-slot-definition (owner slot-name)
  (find-slot-by-owner-type owner (effective-slots-for-slot-name slot-name) nil))

(defgeneric unbound-check-for (syntax)
  (:method (syntax)
           nil)

  (:method ((access slot-access))
           (bind ((type (xtype-of access))
                  (slot (slot-of access))
                  (variable (arg-of access)))
             (debug-only (assert (not (contains-syntax-p type))))
             (when (and slot (query-variable-p variable))
               (cond
                 ((eq type +unknown-type+) (sql-map-failed))
                 ((complex-type-p type) `(sql-is-null ,(sql-bound-column-reference-for slot variable)))
                 ((unbound-subtype-p type) `(sql-is-null ,(sql-column-reference-for slot variable)))
                 (t nil)))))

  (:method ((access association-end-access))
           nil))

(defgeneric null-check-for (syntax)
  (:method (syntax)
           nil)

  (:method ((access slot-access))
           (bind ((type (xtype-of access))
                  (slot (slot-of access))
                  (variable (arg-of access)))
             (debug-only (assert (not (contains-syntax-p type))))
             (if (and slot (query-variable-p variable) (maybe-null-subtype-p type))
                 `(sql-is-null ,(sql-column-reference-for slot variable))
                 nil)))

  (:method ((access association-end-access))
           nil))

;;;----------------------------------------------------------------------------
;;; Helpers
;;;

(defun joined-variable-for-association-end-access (query access)
  (ensure-joined-variable
   query
   (arg-of access)
   (association-end-of access)
   (base-type-for (xtype-of access))))

(defun ensure-joined-variable (query object association-end type)
  (or (and (query-variable-p object) (eq (xtype-of object) type) object)
      (find-joined-variable-by-definition query object association-end type)
      (make-new-joined-variable query object association-end type)))

(defun ensure-type (query object type)
  (if (eq (xtype-of object) +unknown-type+)
      (progn (setf (xtype-of object) type) object)
      (or (and (eq (xtype-of object) type) object)
          (find-joined-variable-by-definition query object nil type)
          (make-new-joined-variable query object nil type))))

(defun find-joined-variable-by-definition (query object association-end type)
  (find-if
   #L(and (typep !1 'joined-variable)
          (eq association-end (association-end-of !1))
          (equal object (object-of !1))
          (eq (xtype-of !1) type))
   (query-variables-of query)))

(defun base-type-for (type)
  (bind ((normalized-type (normalized-type-for* type)))
    (if (eq normalized-type +unknown-type+)
       +unknown-type+
       (cond
          ((set-type-p normalized-type) (find-class (set-type-class-for normalized-type)))
          ((persistent-class-name-p normalized-type) (find-class normalized-type))
          (t normalized-type)))))

(defun make-new-joined-variable (query object association-end type)
  "Creates a new joined variable."
  (bind ((name (generate-joined-variable-name type))
         (variable (make-joined-variable :name name :object object
                                         :association-end association-end :xtype type)))
    (add-joined-variable query variable)
    variable))

(defun generate-joined-variable-name (type)
  "Generates a name for a joined variable of type TYPE."
  (typecase type
    (persistent-class (gensym (symbol-name (class-name type))))
    (symbol (gensym (symbol-name type)))
    (otherwise (gensym "joined"))))

(defgeneric collect-persistent-object-literals (element &optional result)
  (:method ((query simple-query) &optional result)
           (collect-persistent-object-literals
            (order-by-of query)
            (collect-persistent-object-literals
             (action-args-of query)
             (collect-persistent-object-literals
              (asserts-of query)
              result))))

  (:method ((element t) &optional result)
           result)

  (:method ((object persistent-object) &optional result)
           (adjoin object result))

  (:method ((literal literal-value) &optional result)
           (collect-persistent-object-literals (value-of literal) result))

  (:method ((cons cons) &optional result)
           (collect-persistent-object-literals
            (car cons)
            (collect-persistent-object-literals
             (cdr cons)
             result)))

  (:method ((form unparsed-form) &optional result)
           (collect-persistent-object-literals (form-of form) result))

  (:method ((form compound-form) &optional result)
           (collect-persistent-object-literals (operands-of form) result)))

