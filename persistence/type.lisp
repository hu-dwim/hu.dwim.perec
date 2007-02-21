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

(defun type-specifier-p (type)
  (gethash (first (ensure-list type)) *types*))

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

(defvar *canonical-types* nil)

(defun canonical-type-p (type)
  (member (first (ensure-list type)) *canonical-types*))

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
    (boolean 'boolean)
    (double 'double)
    ((?is ?type canonical-type-p)
     type)
    ((?is ?type class-type-p)
     type)
    ((?or (and (?* ?x) ?a (?* ?y) (not ?a) (?* ?z))
          (and (?* ?x) (not ?a) (?* ?y) ?a (?* ?z)))
     nil)
    ((?or (or (?* ?x) ?a (?* ?y) (not ?a) (?* ?z))
          (or (?* ?x) (not ?a) (?* ?y) ?a (?* ?z)))
     t)
    ((and (?* ?x) ?a (?* ?x) ?b (?* ?z)
          (?if (and (persistent-class-type-p ?a)
                    (persistent-class-type-p ?b)
                    (not (intersection (list* (find-class ?a) (persistent-effective-sub-classes-of (find-class ?a)))
                                       (list* (find-class ?b) (persistent-effective-sub-classes-of (find-class ?b))))))))
     nil)
    ((?or (and (?* ?x) ?a (?* ?y) (not ?b) (?* ?z)
               (?if (disjunct-type-p ?a ?b)))
          (and (?* ?x) (not ?b) (?* ?y) ?a (?* ?z)
               (?if (disjunct-type-p ?a ?b))))
     (canonical-type-for `(and ,@?x ,@?y ,?a ,@?z)))
    (((?or or and not) . ?args)
     (list* (first type) (mapcar #'canonical-type-for (cdr type))))
    ((?is ?type symbolp)
     (canonical-type-for (substitute-type-arguments type nil)))
    ((?is ?type type-specifier-p)
     (canonical-type-for (substitute-type-arguments (first type) (cdr type))))
    (?type
     type)))

(defparameter *mapped-type-precedence-list*
  '(nil
    unbound
    null
    boolean
    integer-16
    integer-32
    integer-64
    integer
    float-32
    float-64
    float
    double
    number
    text
    duration
    string
    date
    time
    timestamp
    form
    member
    symbol*
    symbol
    serialized
    t))

;;;;;;;;;
;;; Types

(defmacro defmapping (name sql-type reader writer)
  `(progn
    (defmethod compute-column-type* ((type (eql ',name)) type-specification)
      (declare (ignorable type-specification))
      ,sql-type)

    (defmethod compute-reader* ((type (eql ',name)) type-specification)
      (declare (ignorable type-specification))
      ,reader)

    (defmethod compute-writer* ((type (eql ',name)) type-specification)
      (declare (ignorable type-specification))
      ,writer)))

;;;;;;;
;;; Nil
;;;
;;; other -> (type-error)

(defmapping nil nil
  #L(error 'type-error :datum (first !1) :expected-type nil)
  #L(error 'type-error :datum !1 :expected-type nil))

;;;;;;;;;;;
;;; Unbound
;;; 
;;; unbound -> NULL
;;; t -> type-error

;; this type must be used to mark slots which might be unbound (e.g. (or unbound null integer))
(deftype* unbound ()
  `(member ,+unbound-slot-value+))

(defmapping unbound (make-instance 'sql-boolean-type)
  (unbound-reader #L(error 'type-error :datum (first !1) :expected-type type))
  (unbound-writer #L(error 'type-error :datum !1 :expected-type type)))

;;;;;;;;
;;; Null
;;; 
;;; nil -> NULL
;;; t -> (type-error)

(defmapping null (make-instance 'sql-boolean-type)
  (null-reader #L(error 'type-error :datum (first !1) :expected-type type))
  (null-writer #L(error 'type-error :datum !1 :expected-type type)))

;;;;;
;;; t
;;; 
;;; unbound -> NULL, NULL
;;; nil -> true, NULL
;;; other -> true, (base64)

(defmapping t (make-instance 'sql-character-large-object-type)
  'base64->object-reader
  'object->base64-writer)

;;;;;;;;;;;;;;
;;; Serialized
;;; 
;;; unbound -> (type-error)
;;; nil -> (type-error)
;;; other -> (base64)

(defun maximum-serialized-size-p (serialized)
  (declare (ignore serialized))
  t)

(deftype* serialized (&optional size)
  (declare (ignore size))
  '(and (not unbound)
        (not null)
        (satisfies maximum-serialized-size-p)))

(defmapping serialized (if (consp type-specification)
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

(defmapping boolean (make-instance 'sql-boolean-type)
  'object->boolean-reader
  'boolean->string-writer)

;;;;;;;;;;;;;;
;;; Integer-16
;;;
;;; non integer -> (type-error)

(deftype* integer-16 ()
  `(integer ,(- (expt 2 15)) ,(1- (expt 2 15))))

(defmapping integer-16 (make-instance 'sql-integer-type :bit-size 16)
  'object->integer-reader
  (non-null-identity-writer type))

;;;;;;;;;;;;;;
;;; Integer-32
;;;
;;; non integer -> (type-error)

(deftype* integer-32 ()
  `(integer ,(- (expt 2 31)) ,(1- (expt 2 31))))

(defmapping integer-32 (make-instance 'sql-integer-type :bit-size 32)
  'object->integer-reader
  (non-null-identity-writer type))

;;;;;;;;;;;;;;
;;; Integer-64
;;;
;;; non integer -> (type-error)

(deftype* integer-64 ()
  `(integer ,(- (expt 2 63)) ,(1- (expt 2 63))))

(defmapping integer-64 (make-instance 'sql-integer-type :bit-size 64)
  'object->integer-reader
  (non-null-identity-writer type))

;;;;;;;;;;;
;;; Integer
;;;
;;; non integer -> (type-error)

(defmapping integer (make-instance 'sql-integer-type)
  'object->integer-reader
  (non-null-identity-writer type))

;;;;;;;;;;;;
;;; Float-32
;;;
;;; non float -> (type-error)

(deftype* float-32 ()
  `float)

(defmapping float-32 (make-instance 'sql-float-type :bit-size 32)
  'object->number-reader
  (non-null-identity-writer type))

;;;;;;;;;;;;
;;; Float-64
;;;
;;; non float -> (type-error)

(deftype* float-64 ()
  `float)

(defmapping float-64 (make-instance 'sql-float-type :bit-size 64)
  'object->number-reader
  (non-null-identity-writer type))

;;;;;;;;;
;;; Float
;;;
;;; non float -> (type-error)

(defmapping float (make-instance 'sql-float-type :bite-size 32)
  'object->number-reader
  (non-null-identity-writer type))

;;;;;;;;;;
;;; Double
;;;
;;; non double -> (type-error)

(deftype* double ()
  'double-float)

(defmapping double-float (make-instance 'sql-float-type :bit-size 64)
  'object->number-reader
  (non-null-identity-writer type))

;;;;;;;;;;
;;; Number
;;;
;;; non number -> (type-error)

(defmapping number (make-instance 'sql-numeric-type)
  'object->number-reader
  (non-null-identity-writer type))

;;;;;;;;;;
;;; String
;;;
;;; non string -> (type-error)

(defmapping string (if (consp type-specification)
                       (make-instance 'sql-character-type :size (second type-specification))
                       (make-instance 'sql-character-large-object-type))
  (non-null-identity-reader type)
  (non-null-identity-writer type))

;;;;;;;;
;;; Text
;;;
;;; non string -> (type-error)

;; TODO:
(defun maximum-length-p (string)
  (declare (ignore string))
  t)

(deftype* text (&optional maximum-length)
  (declare (ignore maximum-length))
  '(and string
        (satisfies maximum-length-p)))

(defmapping text (if (consp type-specification)
                     (make-instance 'sql-character-varying-type :size (second type-specification))
                     (make-instance 'sql-character-large-object-type))
  (non-null-identity-reader type)
  (non-null-identity-writer type))

;;;;;;;;;;
;;; Symbol
;;;
;;; non symbol -> (type-error)

(defmapping symbol (make-instance 'sql-character-large-object-type)
  'string->symbol-reader
  'symbol->string-writer)

;; TODO:
(defun maximum-symbol-name-length-p (symbol)
  (declare (ignore symbol))
  t)

(deftype* symbol* (&optional maximum-length)
  (declare (ignore maximum-length))
  '(and symbol
        (satisfies maximum-symbol-name-length-p)))

(defmapping symbol* (if (consp type-specification)
                        (make-instance 'sql-character-varying-type :size (second (find 'symbol* type-specification
                                                                                       :key #L(when (listp !1) (first !1)))))
                        (make-instance 'sql-character-large-object-type))
  'string->symbol-reader
  'symbol->string-writer)

;;;;;;;;
;;; Date
;;;
;;; non date -> (type-error)

;; TODO:
(defun date-p (date)
  (declare (ignore date))
  t)

(deftype* date ()
  '(and local-time
        (satisfies date-p)))

(defmapping date (make-instance 'sql-date-type)
  'integer->local-time-reader
  'local-time->string-writer)

;;;;;;;;
;;; Time
;;;
;;; non date -> (type-error)

;; TODO:
(defun time-p (time)
  (declare (ignore time))
  t)

(deftype* time ()
  '(and local-time
        (satisfies time-p)))

(defmapping time (make-instance 'sql-time-type)
  'string->local-time-reader
  'local-time->string-writer)

;;;;;;;;;;;;;
;;; Timestamp
;;;
;;; non date -> (type-error)

(deftype* timestamp ()
  'local-time)

(defmapping timestamp (make-instance 'sql-timestamp-type)
  'integer->local-time-reader
  'local-time->string-writer)

;;;;;;;;;;;;
;;; Duration
;;;
;;; non string -> (type-error)

;; TODO:
(defun duration-p (duration)
  (declare (ignore duration))
  t)

(deftype* duration ()
  '(and string
        (satisfies duration-p)))

(defmapping duration (make-instance 'sql-character-varying-type :size 32)
  (non-null-identity-reader type)
  (non-null-identity-writer type))

;;;;;;;;
;;; Form
;;;
;;; non form -> (type-error)

(deftype* form (&optional size)
  (declare (ignore size))
  '(and list
        (satisfies maximum-serialized-size-p)))

(defmapping form (make-instance 'sql-character-varying-type)
  'string->list-reader
  'list->string-writer)

;;;;;;;;;;
;;; Member
;;;
;;; not found in members -> (type-error)

(defmapping member (make-instance 'sql-integer-type :bit-size 16)
  (integer->member-reader type-specification)
  (member->integer-writer type-specification))

;;;;;;
;;; Or
;;;
;;; May be used to combine null and unbound with a primitive type and may generate an extra column.
;;; See compute-reader and compute-writer generic function definitions
