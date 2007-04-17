;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;; Type specifier:
;;;;
;;;;   persistent classes (types of persistent objects)
;;;;   names of persistent classes
;;;;   type specifiers for slots (see persistence/standard-type.lisp)
;;;;   and,or,not combinations
;;;;
;;;; Type expression:
;;;;   an expression that evaluates to a type specifier
;;;;


;;;----------------------------------------------------------------------------
;;; Type inference
;;;

(defun infer-types (query)
  "Annotates types to the SYNTAX nodes of the query."
  (mapcan-asserts #L(aif (infer-types-pass-1 !1 query) (list it) nil) query)
  (mapc-query #L(infer-types-pass-2 !1 query #t) query))

(defun mapcan-asserts (fn query)
  (setf (asserts-of query) (mapcan fn (asserts-of query))))

(defun mapc-query (fn query)
  (mapc fn (asserts-of query))
  (mapc fn (action-args-of query))
  (mapc #L(when (syntax-object-p !1) (funcall fn !1)) (order-by-of query)))

(defgeneric infer-types-pass-1 (syntax query)
  (:method (syntax query)
           syntax)

  (:method ((call function-call) query)
           (infer-types-pass-1-function-call (fn-of call) (first (args-of call)) (second (args-of call)) call query))

)

(defgeneric infer-types-pass-1-function-call (fn arg1 arg2 call query)
  (:method (fn arg1 arg2 call query)
           call)

  ;; toplevel (typep <query-variable> <type>)
  (:method ((fn (eql 'typep)) (variable query-variable) type call query)
           (restrict-variable-type variable (type-syntax->type type))
           nil))

(defgeneric infer-types-pass-2 (syntax query &optional toplevel)
  (:method (syntax query &optional toplevel)
           (declare (ignore syntax query toplevel))
           (values))

  (:method ((form compound-form) query &optional toplevel)
           (declare (ignore toplevel))
           (mapc #L(infer-types-pass-2 !1 query #f) (operands-of form)))

  (:method ((access slot-access) query &optional toplevel)
           (declare (ignore toplevel))
           (call-next-method)
           (setf (slot-of access) (slot-for-slot-access access))
           (when (slot-of access)
             (setf (xtype-of access)
                   (slot-definition-type (slot-of access)))))

  ;; toplevel (eq <obj1> <obj2>) -> (type-of <obj1>) == (type-of <obj2>)
  (:method ((call function-call) query &optional toplevel)
           (call-next-method)
           (when (and toplevel
                      (member (fn-of call) '(eq eql equal =
                                             local-time= local-time/= ; TODO these are n-ary
                                             local-time< local-time> local-time<= local-time>=))
                      (= (length (args-of call)) 2))
             (bind ((obj1 (first (args-of call)))
                    (obj2 (second (args-of call))))
               (cond
                 ((and (not (has-default-type-p obj1)) (has-default-type-p obj2))
                  (setf (xtype-of obj2) (xtype-of obj1)))
                 ((and (has-default-type-p obj1) (not (has-default-type-p obj2)))
                  (setf (xtype-of obj1) (xtype-of obj2))))))))

(defun restrict-variable-type (variable type)
  (let ((orig-type (xtype-of variable)))
    (cond
      ((eq orig-type +persistent-object-class+) (setf (xtype-of variable) type))
      ((and (listp orig-type) (eq (first orig-type) 'and)) (appendf (xtype-of variable) type))
      (t (setf (xtype-of variable) (list 'and orig-type type))))))

(defgeneric slot-for-slot-access (access)
  (:method ((access slot-access))
           (find-slot-by-owner-type (arg-of access)
                                    (effective-slots-for-accessor (accessor-of access))
                                    (accessor-of access)))
  (:method ((access association-end-access))
           (find-slot-by-owner-type (arg-of access)
                                    (effective-association-ends-for-accessor (accessor-of access))
                                    (accessor-of access))))

(defun find-slot-by-owner-type (owner slots accessor)
  (flet ((qualified-name-of (slot)
           (concatenate-symbol (class-name (slot-definition-class slot))
                               ":"
                               (slot-definition-name slot))))
    (bind ((owner-type (normalized-type-for* (xtype-of owner))))
     (acond
      ((length=1 slots)
       (first slots))
      ((and (not (eq owner-type +unknown-type+))
            (not (contains-syntax-p owner-type))
            (find owner-type slots :key 'slot-definition-class :test 'subtypep))
       it)
      (t
       (warn "Cannot find the slot for the acccessor ~A.~%Candidates are ~A, owner type is ~A."
             accessor (mapcar #'qualified-name-of slots) owner-type)
       nil)))))

;;;----------------------------------------------------------------------------
;;; Type utils
;;;

(defun type-syntax->type (type)
  (if (and (literal-value-p type)
           (typep (value-of type) 'persistent-class))
      (value-of type)
      type))

(defun subtypep* (sub super)
  (if (or (eq sub +unknown-type+)
          (eq super +unknown-type+)
          (contains-syntax-p sub)
          (contains-syntax-p super))
      (values nil nil)
      (subtypep sub super)))

(defun normalized-type-for* (type)
  (if (or (eq type +unknown-type+)
          (contains-syntax-p type))
      type
      (normalized-type-for type)))

(defun has-default-type-p (syntax)
  (eq (xtype-of syntax)
      (funcall (slot-definition-initfunction
                (find-slot (class-name (class-of syntax)) 'xtype)))))

(defun contains-syntax-p (type)
  (or (typep type 'syntax-object)
      (and (consp type)
           (some #'contains-syntax-p type))))

(defun maybe-null-subtype-p (type)
  (or (eq type +unknown-type+)
      (bind (((values normalized-type null-subtype-p unbound-subtype-p) (destructure-type type)))
        (declare (ignore normalized-type unbound-subtype-p))
        null-subtype-p)))

(defgeneric backquote-type-syntax (type)
  (:documentation "Generates a type expression that evaluates to the type.")

  (:method ((self-evaluating t))
           self-evaluating)
  
  (:method ((class persistent-class))
           class)

  (:method ((type-name symbol))
           `(quote ,type-name))

  (:method ((type syntax-object))
           type)

  (:method ((combined-type list))
           `(list
             ',(first combined-type)
             ,@(mapcar 'backquote-type-syntax (rest combined-type)))))

