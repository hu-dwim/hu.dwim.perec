;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;;
;;; Type

(defparameter *types* (make-hash-table))

(defclass* type-object ()
  ((args
    :type list)
   (body
    :type list)))

(defmacro deftype** (name args &body body)
  `(progn
    (deftype ,name ,args ,@body)
    (setf (gethash ',name *types*) ,@body)))

;;;;;;;;;;;;;
;;; Normalize

(defun normalize (type)
  (iter (for simplified-type initially type then (simplify-boolean-form (normalize* (->dnf simplified-type))))
        (for previous-type previous simplified-type)
        (until (equal simplified-type previous-type))
        (finally (return simplified-type))))

(defun class-type-p (type)
  (and (symbolp type)
       (find-class type nil)))

(defun normalize* (type)
  (pattern-case type
    (#t #t)
    (#f #f)
    ((and (?* ?x) (not ?a) (?* ?y) ?b (?* ?z)
          (?if (not (subtypep ?a ?b))))
     (normalize `(and ,@?x ,@?y ,?b ,@?z)))
    ((and (?* ?x) ?a (?* ?y) (not ?b) (?* ?z)
          (?if (not (subtypep ?b ?a))))
     (normalize `(and ,@?x ,@?y ,?a ,@?z)))
    ((and (?* ?x) ?a (?* ?y) (not ?a) (?* ?z))
     #f)
    ((and (?* ?x) (not ?a) (?* ?y) ?a (?* ?z))
     #f)
    ((or (?* ?x) ?a (?* ?y) (not ?a) (?* ?z))
     #t)
    ((or (?* ?x) (not ?a) (?* ?y) ?a (?* ?z))
     #t)
    ((?is ?type class-type-p)
     type)
    ((?is ?type symbolp)
     (let ((body (gethash type *types*)))
       (normalize body)))
    (((?or or and not) . ?args)
     (list* (first type) (mapcar #'normalize (cdr type))))
    (?type
     type)))

(defun simplify-boolean-form (form)
  "Makes the following simplifications on form:
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
  (pattern-case form
    ((not ?arg)
     (bind ((arg (simplify-boolean-form ?arg)))
       (pattern-case arg
         ((not ?arg) ?arg)
         (#f #t)
         (#t #f)
         (?otherwise form))))
    ((or . ?args)
     (bind ((operands (remove #f (mapcar #'simplify-boolean-form ?args))))
       (cond
         ((null operands) #f)
         ((length=1 operands) (first operands))
         ((some #L(eq !1 #t) operands) #t)
         (t `(or ,@operands)))))
    ((and . ?args)
     (bind ((operands (remove #t (mapcar #'simplify-boolean-form ?args))))
       (cond
         ((null operands) #t)
         ((length=1 operands) (first operands))
         ((some #L(eq !1 #f) operands) #f)
         (t `(and ,@operands)))))
    (?otherwise form)))

#|
;; TODO: example
(deftype** t1 ()
  '(member a b c))

(deftype** unbound ()
  '(member +unbound-slot-value+))

#+nil
(normalize '(and (not null)
             (not unbound)
             (or null unbound t1)))
|#

;;;;;;;;;
;;; Types

(defmacro deftype* (name sql-type reader writer)
  `(progn
    (defmethod compute-column-type ((type (eql ',name)) &optional type-specification)
      (declare (ignorable type-specification))
      ,sql-type)

    (defmethod compute-reader ((type (eql ',name)) &optional type-specification)
      (declare (ignorable type-specification))
      ,reader)

    (defmethod compute-writer ((type (eql ',name)) &optional type-specification)
      (declare (ignorable type-specification))
      ,writer)))

;;;;;;;;;;;
;;; Unbound
;;; 
;;; unbound -> NULL
;;; t -> type-error

;; this type must be used to mark slots which might be unbound (e.g. (or unbound null integer))
(deftype unbound ()
  `(member ,+unbound-slot-value+))

(deftype* unbound (make-instance 'sql-boolean-type)
  (unbound-reader #L(error 'type-error :datum (first !1) :expected-type type))
  (unbound-writer #L(error 'type-error :datum !1 :expected-type type)))

;;;;;;;;
;;; Null
;;; 
;;; nil -> NULL
;;; t -> (type-error)

(deftype* null (make-instance 'sql-boolean-type)
  (null-reader #L(error 'type-error :datum (first !1) :expected-type type))
  (null-writer #L(error 'type-error :datum !1 :expected-type type)))

;;;;;;;
;;; Nil
;;;
;;; other -> (type-error)

(deftype* nil nil
  #L(error 'type-error :datum (first !1) :expected-type nil)
  #L(error 'type-error :datum !1 :expected-type nil))

;;;;;
;;; t
;;; 
;;; unbound -> NULL, NULL
;;; nil -> true, NULL
;;; other -> true, (base64)

(deftype* t (make-instance 'sql-character-large-object-type)
  (unbound-or-null-reader 'base64->object-reader)
  (unbound-or-null-writer 'object->base64-writer 2))

;;;;;;;;;;;;;;
;;; Serialized
;;; 
;;; unbound -> (type-error)
;;; nil -> (type-error)
;;; other -> (base64)

(deftype serialized (&optional size)
  (declare (ignore size))
  '(and (not unbound)
        (not null)))

(deftype* serialized (if (consp type-specification)
                         (make-instance 'sql-character-varying-type :size (second type-specification))
                         (make-instance 'sql-character-large-object-type))
  'base64->object-reader
  'object->base64-writer)

;;;;;;;;;;;
;;; Boolean
;;; 
;;; nil -> false
;;; t -> true
;;; other -> (type-error)

(deftype* boolean (make-instance 'sql-boolean-type)
  'object->boolean-reader
  'identity-writer)

;;;;;;;;;;;;;;
;;; Integer-16
;;;
;;; non integer -> (type-error)

(deftype integer-16 ()
  `(integer ,(- (expt 2 15)) ,(1- (expt 2 15))))

(deftype* integer-16 (make-instance 'sql-integer-type :bit-size 16)
  'object->integer-reader
  (non-null-identity-writer type))

;;;;;;;;;;;;;;
;;; Integer-32
;;;
;;; non integer -> (type-error)

(deftype integer-32 ()
  `(integer ,(- (expt 2 31)) ,(1- (expt 2 31))))

(deftype* integer-32 (make-instance 'sql-integer-type :bit-size 32)
  'object->integer-reader
  (non-null-identity-writer type))

;;;;;;;;;;;;;;
;;; Integer-64
;;;
;;; non integer -> (type-error)

(deftype integer-64 ()
  `(integer ,(- (expt 2 63)) ,(1- (expt 2 63))))

(deftype* integer-64 (make-instance 'sql-integer-type :bit-size 64)
  'object->integer-reader
  (non-null-identity-writer type))

;;;;;;;;;;;
;;; Integer
;;;
;;; non integer -> (type-error)

(deftype* integer (make-instance 'sql-integer-type)
  'object->integer-reader
  (non-null-identity-writer type))

;;;;;;;;;;;;
;;; Float-32
;;;
;;; non float -> (type-error)

(deftype float-32 ()
  `float)

(deftype* float-32 (make-instance 'sql-float-type :bit-size 32)
  'object->number-reader
  (non-null-identity-writer type))

;;;;;;;;;;;;
;;; Float-64
;;;
;;; non float -> (type-error)

(deftype float-64 ()
  `float)

(deftype* float-64 (make-instance 'sql-float-type :bit-size 64)
  'object->number-reader
  (non-null-identity-writer type))

;;;;;;;;;
;;; Float
;;;
;;; non float -> (type-error)

(deftype* float (make-instance 'sql-float-type)
  'object->number-reader
  (non-null-identity-writer type))

;;;;;;;;;;
;;; Double
;;;
;;; non double -> (type-error)

(deftype* double (make-instance 'sql-float-type)
  'object->number-reader
  (non-null-identity-writer type))

;;;;;;;;;;
;;; Number
;;;
;;; non number -> (type-error)

(deftype* number (make-instance 'sql-numeric-type)
  'object->number-reader
  (non-null-identity-writer type))

;;;;;;;;;;
;;; String
;;;
;;; non string -> (type-error)

(deftype* string (if (consp type-specification)
                     (make-instance 'sql-character-varying-type :size (second type-specification))
                     (make-instance 'sql-character-large-object-type))
  (non-null-identity-reader type)
  (non-null-identity-writer type))

;;;;;;;;;;
;;; Symbol
;;;
;;; non symbol -> (type-error)

(deftype* symbol (make-instance 'sql-character-large-object-type)
  'string->symbol-reader
  'symbol->string-writer)

(deftype symbol* (&optional size)
  (declare (ignore size))
  'symbol)

(deftype* symbol* (if (consp type-specification)
                      (make-instance 'sql-character-varying-type :size (second type-specification))
                      (make-instance 'sql-character-large-object-type))
  'string->symbol-reader
  'symbol->string-writer)

;;;;;;;;
;;; Date
;;;
;;; non date -> (type-error)

(deftype date ()
  'local-time)

(deftype* date (make-instance 'sql-date-type)
  'integer->local-time-reader
  'local-time->string-writer)

;;;;;;;;
;;; Time
;;;
;;; non date -> (type-error)

(deftype time ()
  'local-time)

(deftype* time (make-instance 'sql-time-type)
  'string->local-time-reader
  'local-time->string-writer)

;;;;;;;;;;;;;
;;; Timestamp
;;;
;;; non date -> (type-error)

(deftype timestamp ()
  'local-time)

(deftype* timestamp (make-instance 'sql-timestamp-type)
  'integer->local-time-reader
  'local-time->string-writer)

;;;;;;;;;;;;
;;; Duration
;;;
;;; non string -> (type-error)

(deftype duration ()
  'string)

(deftype* duration (make-instance 'sql-character-varying-type :size 32)
  (non-null-identity-reader type)
  (non-null-identity-writer type))

;;;;;;;;
;;; Form
;;;
;;; non form -> (type-error)

(deftype form (&optional size)
  (declare (ignore size))
  'list)

(deftype* form (make-instance 'sql-character-varying-type)
  'string->list-reader
  'list->string-writer)

;;;;;;;;;;
;;; Member
;;;
;;; not found in members -> (type-error)

(deftype* member (make-instance 'sql-integer-type :bit-size 16)
  (integer->member-reader type-specification)
  (member->integer-writer type-specification))

;;;;;;
;;; Or
;;;
;;; May combine null and unbound with a primitive type and may generate an extra column
;;; See compute-reader and compute-writer generic function definitions
