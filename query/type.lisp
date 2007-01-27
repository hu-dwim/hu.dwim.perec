;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;; entity
;;;;
;;;; persistent-object + subclasses
;;;;
;;;; model types of attribute values (string(6),integer(1,10),enum(a,b,c),state(i,e),etc.)
;;;;
;;;; and,or,not combinations
;;;;
;;;; function types
;;;;

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

  (:method ((access property-access) query &optional toplevel)
           (declare (ignore toplevel))
           (call-next-method)
           (when (xtype-of (arg-of access))
             (setf (property-of access) (property-for-property-access access)))
           (when (property-of access)
             (setf (xtype-of access)
                   (if (attribute-access-p access)
                       (slot-definition-type (property-of access))
                       (associated-class-of (property-of access))))))

  (:method ((call function-call) query &optional toplevel)
           (call-next-method)
           (when (and toplevel (eq (fn-of call) 'eq) (= (length (args-of call)) 2))
             (bind ((arg1 (first (args-of call)))
                    (arg2 (second (args-of call)))
                    (type1 (xtype-of arg1))
                    (type2 (xtype-of arg2)))
               (cond
                ((and type1 (not type2)) (setf (xtype-of arg2) type1))
                ((and (not type1) type2) (setf (xtype-of arg1) type2)))))))

(defun type-syntax->type (type)
  (if (and (literal-value-p type)
           (typep (value-of type) 'persistent-class))
      (value-of type)
      type))

(defun restrict-variable-type (variable type)
  (let ((orig-type (xtype-of variable)))
    (cond
      ((eq orig-type +the-persistent-object-class+) (setf (xtype-of variable) type))
      ((and (listp orig-type) (eq (first orig-type) 'and)) (appendf (xtype-of variable) type))
      (t (setf (xtype-of variable) (list 'and orig-type type))))))

(defgeneric property-for-property-access (access)
  (:method ((access attribute-access))
           (find-property-by-owner-type (arg-of access)
                                        (effective-slots-for-accessor (accessor-of access))))
  (:method ((access association-end-access))
           (find-property-by-owner-type (arg-of access)
                                        (effective-association-ends-for-accessor (accessor-of access)))))

(defun find-property-by-owner-type (owner properties)
  (bind ((owner-type (xtype-of owner)))
    (cond
      ((length=1 properties)
       (first properties))
      ((persistent-class-p owner-type)
       (find owner-type properties :key 'slot-definition-class :test 'subtypep))
      (t
       (warn "Cannot determine the type of ~A at compile time.
Chosing property ~A randomly from ~A."
             owner
             (property-qualified-name (first properties))
             (mapcar 'property-qualified-name properties))
       (first properties)))))

(defun property-qualified-name (slot)
  (concatenate-symbol (class-name (slot-definition-class slot)) ":" (slot-definition-name slot)))
