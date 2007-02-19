;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(enable-pattern-reader #\M)

;;;
;;; Partial eval
;;;
(defun partial-eval (syntax query &optional static-vars)
  "Returns the partially evaluated SYNTAX. The SYNTAX can be a SYNTAX-OBJECT or a lisp form
 containing syntax objects. The result is always a SYNTAX-OBJECT."
  (syntax-from-value (%partial-eval-syntax syntax query static-vars) syntax))

(defgeneric %partial-eval-syntax (syntax query static-vars)
  (:documentation
   "Partially evaluates SYNTAX and returns a partially evaluated SYNTAX-OBJECT or the value
if it was fully evaluated.")

  (:method (syntax query static-vars)
           (%partial-eval-syntax (parse-query-form syntax (get-variables query)) query static-vars))

  (:method ((syntax syntax-object) query static-vars)
           (error "Unknown syntax: ~S~%" syntax))

  (:method ((unparsed unparsed-form) query static-vars)
           unparsed)

  (:method ((literal literal-value) query static-vars)
           (value-of literal))

  (:method ((variable variable) query static-vars)
           variable)

  (:method ((variable dynamic-variable) query static-vars)
           (bind ((variable-name (name-of variable)))
             (if (and (boundp variable-name) (member variable-name static-vars))
                 (symbol-value variable-name)
                 variable)))

  (:method ((call macro-call) query static-vars)
           (bind ((args (args-of call)))
             (%partial-eval-macro-call
              (macro-of call) (length args) (first args) (second args) args call query static-vars)))

  (:method ((call function-call) query static-vars)
           (bind ((args (mapcar #L(%partial-eval-syntax !1 query static-vars) (args-of call))))
             (%partial-eval-function-call
              (fn-of call) (length args) (first args) (second args) args call)))

  (:method ((form special-form) query static-vars)
           (%partial-eval-special-form (operator-of form) (operands-of form) form query static-vars)))

(defgeneric %partial-eval-function-call (fn n-args arg-1 arg-2 args call)

  (:method (fn n-args arg-1 arg-2 args call)
           (if (some 'syntax-object-p args)
               (progn (setf (args-of call) (mapcar 'syntax-from-value args (args-of call))) call)
               (apply fn args)))

  ;; (typep query-variable t1) -> nil
  ;;    when the types t1 and (xtype-of query-variable) does not have common subtypes
  (:method ((fn (eql 'typep)) (n-args (eql 2)) (variable query-variable) (type persistent-class) args call)
           (let ((variable-type (xtype-of variable)))
             (if (and (typep variable-type 'persistent-class)
                      (null (intersection (adjoin type (persistent-effective-sub-classes-of type))
                                          (adjoin variable-type (persistent-effective-sub-classes-of variable-type)))))
                 nil
                 (call-next-method))))

  ;; (member x nil) -> nil
  ;; (member x <list>) -> (member x <list2>) where list2 contains those elements of list,
  ;;                                         that have matching type
  (:method ((fn (eql 'member)) (n-args (eql 2)) object (list list) args call)
           (bind ((type (xtype-of object))
                  (list (if (persistent-class-p type) (collect-if #L(typep !1 type) list) list)))
             (cond
               ((null list) nil)
               (t (setf args (list object list))
                  (call-next-method 'member 2 object list args call))))))

(defgeneric %partial-eval-macro-call (macro n-args arg-1 arg-2 args call query static-vars)

  (:method (macro n-args arg-1 arg-2 args call query static-vars)
           call)

  (:method ((macro (eql 'and)) n-args arg-1 arg-2 args call query static-vars)
           (%partial-eval-and/or call query static-vars))

  (:method ((macro (eql 'or)) n-args arg-1 arg-2 args call query static-vars)
           (%partial-eval-and/or call query static-vars)))

(defun %partial-eval-and/or (call query static-vars)
  (bind ((args (mapcar #L(%partial-eval-syntax !1 query static-vars) (args-of call))))
             (if (some 'syntax-object-p args)
                (progn (setf (args-of call) (mapcar 'syntax-from-generalized-boolean args))
                       (simplify-boolean-syntax call))
                (eval (cons (macro-of call) (mapcar 'boolean-from-generalized-boolean args))))))

(defgeneric %partial-eval-special-form (operator args form query static-vars)
  ;; special forms (currently not evaluated, TODO) 
  (:method (operator args form query static-vars)
           form))

(defun syntax-from-value (value orig-syntax)
  (cond
    ((syntax-object-p value) value)
    ((syntax-object-p orig-syntax) (make-literal-value :value value :xtype (xtype-of orig-syntax)))
    (t (make-literal-value :value value))))

(defun syntax-from-generalized-boolean (value)
  (if (syntax-object-p value)
      value
      (make-literal-value :value (if value #t #f))))

(defun boolean-from-generalized-boolean (value)
  (assert (not (syntax-object-p value)))
  (if value #t #f))

(defun is-true-literal (syntax)
  "Returns #t if SYNTAX is a true literal as generalized boolean."
  (and (typep syntax 'literal-value)
       (not (eq (value-of syntax) #f))))

(defun is-false-literal (syntax)
  "Returns #t if SYNTAX is a false literal."
  (and (typep syntax 'literal-value)
       (eq (value-of syntax) #f)))

(defun simplify-boolean-syntax (syntax)
  "Makes the following simplifications on SYNTAX:
   (not false)                -> true
   (not true)                 -> false
   (not (not x))              -> x
   (or)                       -> false
   (or x)                     -> x
   (or x... false y...)       -> (or x... y...)
   (or x... true y...)        -> true
   (or x... (or y...) z...)   -> (or x... y... z...)
   (and)                      -> true
   (and x)                    -> x
   (and x... true y...)       -> (and x... y...)
   (and x... false y...)      -> false
   (and x... (and y...) z...) -> (and x... y... z...)

where x, y and z are arbitrary objects and '...' means zero or more occurence,
and false/true means a generalized boolean literal."
  
  (flet ((simplify-args (operator args)
           (iter (for arg in args)
                 (for simplified = (simplify-boolean-syntax arg))
                 (if (and (macro-call-p simplified) (eq (macro-of simplified) operator))
                     (appending (args-of simplified))
                     (collect simplified)))))
    (pattern-case syntax
      (#M(function-call :fn not :args (?arg))
         (bind ((arg (simplify-boolean-syntax ?arg)))
           (pattern-case arg
             (#M(function-call :fn not :args (?arg)) ?arg)
             (#M(literal-value :value #f) (make-literal-value :value #t))
             (#M(literal-value :value ?true) (make-literal-value :value #f))
             (?otherwise syntax))))
      (#M(macro-call :macro or :args ?args)
         (bind ((operands (remove-if 'is-false-literal (simplify-args 'or ?args))))
           (cond
             ((null operands) (make-literal-value :value #f))
             ((length=1 operands) (first operands))
             ((find-if 'is-true-literal operands) (make-literal-value :value #t))
             (t (make-macro-call :macro 'or :args operands)))))
      (#M(macro-call :macro and :args ?args)
         (bind ((operands (remove-if 'is-true-literal (simplify-args 'and ?args))))
           (cond
             ((null operands) (make-literal-value :value #t))
             ((length=1 operands) (first operands))
             ((find-if 'is-false-literal operands) (make-literal-value :value #f))
             (t (make-macro-call :macro 'and :args operands)))))
      (?otherwise syntax))))
