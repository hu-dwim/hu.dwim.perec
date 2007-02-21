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

(defmacro deftype* (name args &body body)
  `(progn
    (setf (gethash ',name *types*) (make-instance 'type-object :args ',args :body ',body))
    (deftype ,name ,args ,@body)))

(defun substitute-type-arguments (type args)
  (let ((type-object (gethash type *types*)))
    (if type-object
        (let ((body (body-of type-object)))
          (eval `(let (,@(mapcar #L(list !1 !2) (args-of type-object) args))
                  ,@body)))
        (error "Unknown type specifier: ~A" type))))

;;;;;;;;;;;;;
;;; Normalize

(defun canonical-type-for (type)
  (iter (for simplified-type :initially type :then (simplify-boolean-form (canonical-type-for* (->dnf simplified-type))))
        (for previous-type :previous simplified-type)
        (until (equal simplified-type previous-type))
        (finally (return simplified-type))))

(defun class-type-p (type)
  (and (symbolp type)
       (find-class type nil)))

(defun disjunct-type-p (type-1 type-2)
  (equal '(#t #t)
         (multiple-value-list
             (subtypep (canonical-type-for `(and ,type-1 ,type-2)) nil))))

(defun canonical-type-for* (type)
  (pattern-case type
    (t t)
    (nil nil)
    ((?is ?type class-type-p)
     type)
    ((?or (and (?* ?x) ?a (?* ?y) (not ?a) (?* ?z))
          (and (?* ?x) (not ?a) (?* ?y) ?a (?* ?z)))
     nil)
    ((?or (or (?* ?x) ?a (?* ?y) (not ?a) (?* ?z))
          (or (?* ?x) (not ?a) (?* ?y) ?a (?* ?z)))
     t)
    ((?is ?type symbolp)
     (canonical-type-for (substitute-type-arguments type nil)))
    ((?or (and (?* ?x) ?a (?* ?y) (not ?b) (?* ?z)
               (?if (disjunct-type-p ?a ?b)))
          (and (?* ?x) (not ?b) (?* ?y) ?a (?* ?z)
               (?if (disjunct-type-p ?a ?b))))
     (canonical-type-for `(and ,@?x ,@?y ,?a ,@?z)))
    (((?or or and not) . ?args)
     (list* (first type) (mapcar #'canonical-type-for (cdr type))))
    (?type
     type)))

;;;;;;;;;
;;; Types

#+nil
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
(deftype* unbound ()
  `(member ,+unbound-slot-value+))

#+nil
(deftype* unbound (make-instance 'sql-boolean-type)
  (unbound-reader #L(error 'type-error :datum (first !1) :expected-type type))
  (unbound-writer #L(error 'type-error :datum !1 :expected-type type)))

;;;;;;;;
;;; Null
;;; 
;;; nil -> NULL
;;; t -> (type-error)

#+nil
(deftype* null (make-instance 'sql-boolean-type)
  (null-reader #L(error 'type-error :datum (first !1) :expected-type type))
  (null-writer #L(error 'type-error :datum !1 :expected-type type)))

;;;;;;;
;;; Nil
;;;
;;; other -> (type-error)

#+nil
(deftype* nil nil
  #L(error 'type-error :datum (first !1) :expected-type nil)
  #L(error 'type-error :datum !1 :expected-type nil))

;;;;;
;;; t
;;; 
;;; unbound -> NULL, NULL
;;; nil -> true, NULL
;;; other -> true, (base64)

#+nil
(deftype* t (make-instance 'sql-character-large-object-type)
  (unbound-or-null-reader 'base64->object-reader)
  (unbound-or-null-writer 'object->base64-writer 2))

;;;;;;;;;;;;;;
;;; Serialized
;;; 
;;; unbound -> (type-error)
;;; nil -> (type-error)
;;; other -> (base64)

(deftype* serialized (&optional size)
  (declare (ignore size))
  '(and (not unbound)
        (not null)))

#+nil
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

#+nil
(deftype* boolean (make-instance 'sql-boolean-type)
  'object->boolean-reader
  'identity-writer)

;;;;;;;;;;;;;;
;;; Integer-16
;;;
;;; non integer -> (type-error)

(deftype* integer-16 ()
  `(integer ,(- (expt 2 15)) ,(1- (expt 2 15))))

#+nil
(deftype* integer-16 (make-instance 'sql-integer-type :bit-size 16)
  'object->integer-reader
  (non-null-identity-writer type))

;;;;;;;;;;;;;;
;;; Integer-32
;;;
;;; non integer -> (type-error)

(deftype* integer-32 ()
  `(integer ,(- (expt 2 31)) ,(1- (expt 2 31))))

#+nil
(deftype* integer-32 (make-instance 'sql-integer-type :bit-size 32)
  'object->integer-reader
  (non-null-identity-writer type))

;;;;;;;;;;;;;;
;;; Integer-64
;;;
;;; non integer -> (type-error)

(deftype* integer-64 ()
  `(integer ,(- (expt 2 63)) ,(1- (expt 2 63))))

#+nil
(deftype* integer-64 (make-instance 'sql-integer-type :bit-size 64)
  'object->integer-reader
  (non-null-identity-writer type))

;;;;;;;;;;;
;;; Integer
;;;
;;; non integer -> (type-error)

#+nil
(deftype* integer (make-instance 'sql-integer-type)
  'object->integer-reader
  (non-null-identity-writer type))

;;;;;;;;;;;;
;;; Float-32
;;;
;;; non float -> (type-error)

(deftype* float-32 ()
  `float)

#+nil
(deftype* float-32 (make-instance 'sql-float-type :bit-size 32)
  'object->number-reader
  (non-null-identity-writer type))

;;;;;;;;;;;;
;;; Float-64
;;;
;;; non float -> (type-error)

(deftype* float-64 ()
  `float)

#+nil
(deftype* float-64 (make-instance 'sql-float-type :bit-size 64)
  'object->number-reader
  (non-null-identity-writer type))

;;;;;;;;;
;;; Float
;;;
;;; non float -> (type-error)

#+nil
(deftype* float (make-instance 'sql-float-type)
  'object->number-reader
  (non-null-identity-writer type))

;;;;;;;;;;
;;; Double
;;;
;;; non double -> (type-error)

#+nil
(deftype* double (make-instance 'sql-float-type)
  'object->number-reader
  (non-null-identity-writer type))

;;;;;;;;;;
;;; Number
;;;
;;; non number -> (type-error)

#+nil
(deftype* number (make-instance 'sql-numeric-type)
  'object->number-reader
  (non-null-identity-writer type))

;;;;;;;;;;
;;; String
;;;
;;; non string -> (type-error)

#+nil
(deftype* string (if (consp type-specification)
                     (make-instance 'sql-character-varying-type :size (second type-specification))
                     (make-instance 'sql-character-large-object-type))
  (non-null-identity-reader type)
  (non-null-identity-writer type))

;;;;;;;;;;
;;; Symbol
;;;
;;; non symbol -> (type-error)

#+nil
(deftype* symbol (make-instance 'sql-character-large-object-type)
  'string->symbol-reader
  'symbol->string-writer)

(deftype* symbol* (&optional size)
  (declare (ignore size))
  'symbol)

#+nil
(deftype* symbol* (if (consp type-specification)
                      (make-instance 'sql-character-varying-type :size (second type-specification))
                      (make-instance 'sql-character-large-object-type))
  'string->symbol-reader
  'symbol->string-writer)

;;;;;;;;
;;; Date
;;;
;;; non date -> (type-error)

(deftype* date ()
  'local-time)

#+nil
(deftype* date (make-instance 'sql-date-type)
  'integer->local-time-reader
  'local-time->string-writer)

;;;;;;;;
;;; Time
;;;
;;; non date -> (type-error)

(deftype* time ()
  'local-time)

#+nil
(deftype* time (make-instance 'sql-time-type)
  'string->local-time-reader
  'local-time->string-writer)

;;;;;;;;;;;;;
;;; Timestamp
;;;
;;; non date -> (type-error)

(deftype* timestamp ()
  'local-time)

#+nil
(deftype* timestamp (make-instance 'sql-timestamp-type)
  'integer->local-time-reader
  'local-time->string-writer)

;;;;;;;;;;;;
;;; Duration
;;;
;;; non string -> (type-error)

(deftype* duration ()
  'string)

#+nil
(deftype* duration (make-instance 'sql-character-varying-type :size 32)
  (non-null-identity-reader type)
  (non-null-identity-writer type))

;;;;;;;;
;;; Form
;;;
;;; non form -> (type-error)

(deftype* form (&optional size)
  (declare (ignore size))
  'list)

#+nil
(deftype* form (make-instance 'sql-character-varying-type)
  'string->list-reader
  'list->string-writer)

;;;;;;;;;;
;;; Member
;;;
;;; not found in members -> (type-error)

#+nil
(deftype* member (make-instance 'sql-integer-type :bit-size 16)
  (integer->member-reader type-specification)
  (member->integer-writer type-specification))

;;;;;;
;;; Or
;;;
;;; May combine null and unbound with a primitive type and may generate an extra column
;;; See compute-reader and compute-writer generic function definitions
