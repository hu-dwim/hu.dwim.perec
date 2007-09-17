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
  (process-toplevel-typep-asserts query)
  (mapc-query #L(%infer-types !1 query #t) query)
  (mapc-query #'check-types query))

(defun process-toplevel-typep-asserts (query)
  (setf (asserts-of query)
        (mapcan
         (lambda (assert)
           (cond
             ((and (function-call-p assert)
                   (eq (fn-of assert) 'typep)
                   (query-variable-p (first (args-of assert))))
              (restrict-variable-type (first (args-of assert))
                                      (type-syntax->type (second (args-of assert))))
              nil)
             (t
              (list assert))))
         (asserts-of query))))

(defgeneric %infer-types (syntax query &optional toplevel)
  (:method (syntax query &optional toplevel)
           (declare (ignore query toplevel))
           (xtype-of syntax))

  (:method ((form compound-form) query &optional toplevel)
           (declare (ignore toplevel))
           (mapc #L(%infer-types !1 query #f) (operands-of form))
           (xtype-of form))

  (:method ((access slot-access) query &optional toplevel)
           (declare (ignore toplevel))
           (call-next-method)
           (unless (slot-of access)
             (setf (slot-of access)
                   (find-slot-for-access access (slots-for-slot-access access))))
           (when (slot-of access)
             (setf (xtype-of access)
                   (slot-definition-type (slot-of access))))
           (xtype-of access))

  ;; toplevel (eq <obj1> <obj2>)    -> (type-of <obj1>) == (type-of <obj2>)
  ;; toplevel (member <obj1> <obj2>) -> (type-of <obj1>) <= (x-element-type-of <obj2>)
  (:method ((call function-call) query &optional toplevel)
           (call-next-method)
           (when toplevel
             (case (fn-of call)
               ((eq eql equal
                    = local-time= local-time/=
                    local-time< local-time> local-time<= local-time>=)
                (bind ((arg-with-known-type (find-if (lambda (arg) (not (has-default-type-p arg)))
                                                     (args-of call))))
                  (when arg-with-known-type
                    (iter (for arg in (args-of call))
                          (when (has-default-type-p arg)
                            (setf (xtype-of arg) (xtype-of arg-with-known-type))
                            (%infer-types arg query))))))
               (member
                (when (= (length (args-of call)) 2)
                  (bind ((obj1 (first (args-of call)))
                         (obj2 (second (args-of call))))
                    (cond
                      ((and (not (has-default-type-p obj1)) (has-default-type-p obj2))
                       (setf (xtype-of obj2) `(set ,(xtype-of obj1)))
                       (%infer-types obj2 query))
                      ((and (has-default-type-p obj1) (not (has-default-type-p obj2)))
                       (setf (xtype-of obj1) (x-element-type-of (xtype-of obj2)))
                       (%infer-types obj1 query))))))))
           (xtype-of call)))

(defun restrict-variable-type (variable type)
  (let ((orig-type (xtype-of variable)))
    (cond
      ((eq orig-type +persistent-object-class+) (setf (xtype-of variable) type))
      ((and (listp orig-type) (eq (first orig-type) 'and)) (appendf (xtype-of variable) type))
      (t (setf (xtype-of variable) (list 'and orig-type type))))))

(defgeneric slots-for-slot-access (access)
  (:method ((access slot-access))
           (effective-slots-for-accessor (accessor-of access)))
  
  (:method ((access association-end-access))
           (effective-association-ends-for-accessor (accessor-of access))))

(defun find-slot-for-access (access slots)
  (or (find-slot-by-owner-type (arg-of access) slots)
      (find-slot-by-slot-type (xtype-of access) slots)))

(defun find-slot-by-owner-type (owner slots)
  (bind ((owner-type (normalized-type-for* (xtype-of owner))))
    (acond
     ((length=1 slots)
      (first slots))
     ((and (not (eq owner-type +unknown-type+))
           (not (contains-syntax-p owner-type)))
      (find owner-type slots :key 'slot-definition-class :test 'subtypep)))))

(defun find-slot-by-slot-type (type slots)
  (cond
    ((length=1 slots) (first slots))
    ((and (not (eq type +unknown-type+))
          (not (contains-syntax-p type)))
     (find type slots :key 'slot-definition-type :test #L(subtypep (normalized-type-for* !2) !1)))))

(defun x-element-type-of (type)
  (if (and (consp type)
           (eq (first type) 'set))
      (or (second type) +unknown-type+)
      nil))

(defgeneric check-types (syntax)

  (:method (syntax)
           (values))

  (:method ((form compound-form))
           (mapc #'check-types (operands-of form)))

  (:method ((access slot-access))
           (when (null (slot-of access))
             (slot-not-found-warning access (slots-for-slot-access access)))))

(defun slot-not-found-warning (access slots)
  (flet ((qualified-name-of (slot)
           (concatenate-symbol (class-name (slot-definition-class slot))
                               ":"
                               (slot-definition-name slot))))
    (if slots
        (warn 'ambiguous-slot-warning
              :accessor (accessor-of access)
              :access-type (xtype-of access)
              :arg-type (xtype-of (arg-of access))
              :slot-names (mapcar #'qualified-name-of slots))
        (warn 'slot-not-found-warning
              :accessor (accessor-of access)
              :access-type (xtype-of access)
              :arg-type (xtype-of (arg-of access))))))

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

