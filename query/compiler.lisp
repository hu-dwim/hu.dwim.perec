;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;(declaim-debug)

(enable-pattern-reader #\M)

;;;; TODO: subqueries
;;;; TODO: return nil if there is contradiction between asserts
;;;; TODO: eliminate tautologies from asserts
;;;; TODO: update
;;;; TODO: recursive selects
;;;; TODO: evaluate volatile expressions only once

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
      (macroexpand-query
       compiler
       query))))))

(defgeneric macroexpand-query (compiler query)
  (:documentation "Expands macros in the body of the query.")

  (:method (compiler (query query))
           (setf (asserts-of query) (mapcar 'query-macroexpand (asserts-of query)))
           query))

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
  (:documentation "Query compiler that does not optimize sql queries."))

(defmethod emit-query ((compiler trivial-query-compiler) query)
  (bind ((lexical-variables (lexical-variables-of query))
         (variables (get-query-variable-names query))
         (asserts (asserts-of query))
         (action (case (action-of query)
                   (:collect `(collect ,@(action-args-of query)))
                   (:purge `(purge ,@(action-args-of query)))))
         (body (if asserts
                   `(if (and ,@asserts) ,action)
                   action)))
    (with-unique-names (objects result-list)
      `(lambda ,lexical-variables
        (declare (ignorable ,@lexical-variables))
        (let ((,objects (map 'list 'cache-instance
                             (execute ,(sql-select-oids-for-class 'persistent-object))))
              (,result-list nil))
          (flet ((collect (&rest exprs) (push exprs ,result-list))
                 (purge (&rest objects) (mapc 'make-transient objects)))
            (declare (ignorable (function collect) (function purge)))
            (bind-cartesian-product (,variables ,objects)
              ,(with-reloading-persistent-objects body)))
          ,(add-conversion-to-result-type
            query
            (add-unique-filter
             query
             `(make-list-result-set (nreverse ,result-list))))))))) ;; TODO group-by,having,order-by

(defun add-unique-filter (query form)
  (if (uniquep query)
      `(make-unique-result-set ,form)
      form))

(defun add-conversion-to-result-type (query form)
  (ecase (result-type-of query)
    (list `(to-list ,form :flatp ,(flatp query)))
    (scroll `(to-scroll ,form :flatp ,(flatp query)))))

;;;;
;;;; Debug query compiler
;;;;
(defclass* debug-query-compiler (query-compiler)
  ()
  (:documentation "Query compiler which compiles the query with trivial-query-compiler and simple-query-compuler and checks that the results of the compiled queries matches. (used only for testing)"))

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
             (cerror "Return the expected result." 'query-result-mismatch-error
                     :query ,query :result ,result-list :expected ,expected-list))
           ,expected))))))


;;;;---------------------------------------------------------------------------
;;;; Simple query compiler
;;;;
(defclass* simple-query-compiler (query-compiler)
  ()
  (:documentation "Query compiler that can transform queries to SQL."))

(defmethod emit-query ((compiler simple-query-compiler) (query query))
  (bind ((lexical-variables (lexical-variables-of query)))
    `(lambda ,lexical-variables
      (declare (ignorable ,@lexical-variables))
      ,(with-reloading-persistent-objects
        (compile-plan (optimize-plan (generate-plan query)))))))

(defun with-reloading-persistent-objects (form)
  (bind ((objects (collect-persistent-object-literals form))
         (variables (mapcar #L(gensym (symbol-name (class-name (class-of !1)))) objects))
         (substitutions (mapcar 'cons objects variables))
         (bindings (mapcar #L`(,(cdr !1) (load-instance ,(car !1))) substitutions)))
    (if objects
        `(let ,bindings
          ,(substitute-syntax form substitutions))
        form)))

;;;;---------------------------------------------------------------------------
;;;; Transformations of the simle query compiler
;;;;
(defmethod transform-query ((compiler simple-query-compiler) (query query))
  "Transforms the QUERY by pushing down the asserts to the SQL query."
  (parse-query-expressions query)
  (normalize-query query)
  (infer-types query)
  (normalize-association-end-access query)
  (introduce-joined-variables query)
  (add-prefetched-types query)
  (partial-eval-query query)
  query)

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
  (subtypep (class-of <obj>) <type>)         -> (typep <object> <type>)")
  
  (:method (syntax)
    syntax)
  
  (:method ((subselect subselect))
    (normalize-query subselect)
    subselect)
  
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
      (?otherwise
       call))))

(defun normalize-association-end-access (query)
  "If the assoc is 1-1
  (eq (<secondary-assoc-end-accessor> <obj1>) <obj2>) -> (eq (primary-assoc-end-accessor <obj2>) <obj1>)"
  (map-query
   (lambda (slot expr)
     (case slot
       ((:assert :having) (%normalize-association-end-access expr))
       (t expr)))
   query))

(defgeneric %normalize-association-end-access (syntax)
  (:method (syntax)
    syntax)
  
  (:method ((form compound-form))
    (setf (operands-of form)
          (mapcar '%normalize-association-end-access (operands-of form)))
    form)

  (:method ((subselect subselect))
    (normalize-association-end-access subselect))
  
  (:method ((call function-call))
    (pattern-case call
      (#M(function-call :fn eq
                        :args (?or ((?is ?access association-end-access-p) ?object)
                                   (?object (?is ?access association-end-access-p))))
         (if (association-end-of ?access)
             (bind ((association-end (association-end-of ?access))
                    (association (association-of association-end))
                    (other-end (other-association-end-of association-end)))
               (ecase (association-kind-of association)
                 (:1-1
                  (if (primary-association-end-p association-end)
                      call
                      (make-function-call ;; reverse, FIXME check NULL
                       :persistent-type (persistent-type-of call)
                       :fn 'eq
                       :args (list
                              (make-association-end-access
                               :persistent-type (persistent-type-of (arg-of ?access))
                               :association-end other-end
                               :accessor (reader-name-of other-end)
                               :args (list ?object))
                              (arg-of ?access)))))
                 (:1-n
                  call)
                 (:m-n
                  call)))
             call))
      (?otherwise
       call))))

(defun introduce-joined-variables (query)
  (mapc-query (lambda (expr) (introduce-joined-variables-for expr query)) query))

(defgeneric introduce-joined-variables-for (syntax query)
  (:documentation "Substitutes the arguments of slot accessor forms with joined variables.")
  ;; atoms, unparsed
  (:method (syntax query)
    (values))
  ;; subselect
  (:method ((subselect subselect) query)
    (introduce-joined-variables subselect))
  ;; recurse on compound forms
  (:method ((syntax compound-form) query)
    (mapc #L(introduce-joined-variables-for !1 query) (operands-of syntax)))  
  ;; slot access -> ensure that arg is a query variable with the correct type
  (:method ((access slot-access) query)
    (call-next-method)
    (when (association-end-access-p (arg-of access)) ;; TODO check 1-ary end
      (setf (arg-of access)
            (joined-variable-for-association-end-access query (arg-of access))))
    (when (slot-of access)
      (setf (arg-of access)
            (ensure-type query (arg-of access) (slot-definition-class (slot-of access)))))
    (values)))

(defun add-prefetched-types (query)
  (when (eq (prefetch-mode-of query) :all)
    (dolist (variable (query-variables-of query))
      (bind ((type (persistent-type-of variable)))
        (when (persistent-class-p type)
          (dolist (class (persistent-effective-superclasses-of type))
            (when (and (primary-table-of class)
                       (prefetched-slots-of class))
              (ensure-variable-type variable class))))))))

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
;;; Helpers
;;;

(defun joined-variable-for-association-end-access (query access)
  (ensure-joined-variable
   query
   (arg-of access)
   (association-end-of access)
   (base-type-for (persistent-type-of access))))

(defun ensure-joined-variable (query object association-end type)
  (or (and (query-variable-p object) (eq (persistent-type-of object) type) object)
      (find-joined-variable-by-definition query object association-end type)
      (make-new-joined-variable query object association-end type)))

(defun ensure-type (query object type)
  (acond
    ((eq (persistent-type-of object) +unknown-type+)
     (progn (setf (persistent-type-of object) type) object)) ; FIXME?

    ((eq (persistent-type-of object) type)
     object)

    ((query-variable-p object)
     (ensure-variable-type object type)
     object)

    ((find-joined-variable-by-definition query object nil type)
     it)

    (t
     (make-new-joined-variable query object nil type))))

(defun ensure-variable-type (variable type)
  (unless (find type (joined-types-of variable))
    (push type (joined-types-of variable))))

(defun find-joined-variable-by-definition (query object association-end type)
  (find-if
   #L(and (typep !1 'joined-variable)
          (eq (association-end-of !1) association-end)
          (syntax= (object-of !1) object)
          (eq type (persistent-type-of !1)))
   (query-variables-of query)))

(defun base-type-for (type)
  (bind ((normalized-type (normalized-type-for* type)))
    (if (eq normalized-type +unknown-type+)
       +unknown-type+
       (cond
          ((set-type-p* normalized-type) (find-class (set-type-class-for normalized-type)))
          ((persistent-class-name-p normalized-type) (find-class normalized-type))
          (t normalized-type)))))

(defun make-new-joined-variable (query object association-end type)
  "Creates a new joined variable."
  (bind ((name (generate-joined-variable-name type))
         (variable (make-joined-variable :name name :object object
                                         :association-end association-end :persistent-type type)))
    (add-joined-variable query variable)
    variable))

(defun generate-joined-variable-name (type)
  "Generates a name for a joined variable of type TYPE."
  (typecase type
    (persistent-class (gensym (symbol-name (class-name type))))
    (symbol (gensym (symbol-name type)))
    (otherwise (gensym "joined"))))

(defgeneric collect-persistent-object-literals (element &optional result)

  (:method ((element t) &optional result)
           result)
#|
  (:method ((query query) &optional result)
           (collect-persistent-object-literals
            (order-by-of query)
            (collect-persistent-object-literals
             (action-args-of query)
             (collect-persistent-object-literals
              (asserts-of query)
              result))))
|#
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

