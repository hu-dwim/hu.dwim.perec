(in-package :cl-perec)

;;;;;;;;;;;;;;;;;
;; Query frontend

(defmacro select (&whole select-form (&rest variables) &body body &environment env)
  "Selects object from the model.

  Syntax:

     select [<options>] (<var-spec>*) <assert-clause>* <collect-clause> [<order-by-clause>]

     <options>:         (&key result-type flatp uniquep)
     <var-spec>:        <symbol> | (<symbol> <type-expr>)
     <assert-clause>:   (assert <bool-expr>)
     <collect-clause>:  (collect <expr>*)
     <order-by-clause>: (order-by <order-spec>*)
     <order-spec>:      :asc|:desc <expr>

  Semantics:

     The symbols of the form are bound to all objects in the database sequentially.
     Then the asserts are evaluated. If all asserts are satisfied then the expressions
     of the collect clause are added to the result. Finally the result is sorted according
     to the order-by-clause.

     Options may modify how the result is collected:

     result-type: (member 'list 'scroll)
        If the value is 'scroll then the result of the query returned as an instance
        of the 'scroll class. If the value is 'list the the result is a list.
        Default is 'list.

     flatp: generalized-boolean
        If true and the result-type is 'list then result is a flattened list, i.e. the 
        lists returned by the collect clause are appended rather than added to the result.
        Default is true for one element collect clauses, false otherwise.

     uniquep: generalized-boolean
        If true then the value of the collect clause will not be added to the result,
        when it is equal to a previously seen value.

     prefetchp: generalized-boolean
        If true then the values of slots of the returned objects are cached in the object.
        Default is true.

  Example:

     (let ((yesterday (day-before-today)))
       (select ((topic topic) message) 
         (assert (typep message 'message))
         (assert (eq (topic-of message) topic))
         (assert (after (date-of message) yesterday))
         (collect (name-of topic) message)))"
  (declare (ignore variables body))
  (let* ((lexical-variables (remove-duplicates (arnesi::lexical-variables env))))
    `(execute-query
      (make-query ',select-form ',lexical-variables)
      ,@lexical-variables)))

(defmacro simple-select (options variable &body body)
  (bind ((variable-specification
          (typecase variable
            (null '-object-)
            (symbol `(-object- ,variable))
            (t variable)))
         (variable-name (first (ensure-list variable-specification))))
    `(select ,options (,variable-specification)
      ,@(append
         (mapcar #L`(assert ,!1) body)
         `((collect ,variable-name))))))

(defmacro select-first-matching (&optional variable &body body)
  `(let ((scroll (simple-select (:result-type scroll) ,variable ,@body)))
    (when (> (element-count scroll) 0)
      (setf (page-size scroll) 1)
      (first-page! scroll)
      (first (aref (elements scroll) 0)))))

(defmacro select-last-matching (&optional variable &body body)
  `(let ((scroll (simple-select (:result-type scroll) ,variable ,@body)))
    (when (> (element-count scroll) 0)
      (setf (page-size scroll) 1)
      (last-page! scroll)
      (first (aref (element scroll) 0)))))

(defun select-similar-assert-for (type rest)
  ;; PORT:
  (declare (ignore type rest))
  #+nil(bind ((class (find-class type)))
         (iter (for (initarg value) on rest by 'cddr)
               (collect `(equal (,(accessor-name-of (effective-property-for-initarg class initarg)) -object-) ,value)))))

(defmacro select-similar-object (type &rest rest &key &allow-other-keys)
  `(select-object (-object- ,type)
    ,@(select-similar-assert-for type rest)))

(defmacro select-similar-objects (type &rest rest &key &allow-other-keys)
  `(select-objects (-object- ,type)
    ,@(select-similar-assert-for type rest)))

(defmacro select-object (&optional variable &body body)
  `(let ((scroll (simple-select (:result-type scroll) ,variable ,@body)))
    (setf (page-size scroll) 1)
    (case (element-count scroll)
      (0 nil)
      (1 (first-page! scroll) (first (aref (elements scroll) 0)))
      (otherwise (error "Query did not return unique result.")))))

(defmacro select-objects (&optional variable &body body)
  "Select objects using one variable and collect the values of that variable based upon a set of asserts."
  `(simple-select (:result-type list) ,variable ,@body))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Execute and compile

(defgeneric execute-query (query &rest lexical-variable-values)
  ;; PORT:
  (:documentation "TODO:"))

(defgeneric compile-query (query)
  (:documentation "Compiles the query to lisp code that executes the query."))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Query builder interface

(defgeneric make-query (select-form &optional lexical-variables)
  (:documentation
   "Creates a query object from the SELECT-FORM.
When the SELECT-FORM is NIL, an empty query created which can be modified by
ADD-LEXICAL-VARIABLE, ADD-QUERY-VARIABLE, ADD-ASSERT and ADD-COLLECT"))

(defgeneric add-lexical-variable (query variable)
  (:documentation
   "Add a lexical variable named VARIABLE to the QUERY.
Lexical variables can be referenced in the asserts and collects of the query and their
values are passed to EXECUTE-QUERY in the order they are added to the QUERY."))

(defgeneric add-query-variable (query variable)
  (:documentation
   "Add a query variable named VARIABLE to the QUERY.
Query variables can be referenced in the asserts and collects of the QUERY."))

(defgeneric add-assert (query condition)
  (:documentation
   "Add an assert for the CONDITION form to the QUERY."))

(defgeneric add-collect (query expression)
  (:documentation
   "Add a collect for the EXPRESSION form to the QUERY."))

(defgeneric add-order-by (query expression &optional direction)
  (:documentation
   "Add an order-by clause specified by EXPRESSION and DIRECTION to the QUERY."))

(defgeneric set-order-by (query expression &optional direction)
  (:documentation
   "Set an order-by clause specified by EXPRESSION and DIRECTION to the QUERY."))
