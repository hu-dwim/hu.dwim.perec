;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

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

;;;;;
;;; t
;;; 
;;; unbound -> NULL
;;; nil -> 'NIL'
;;; other -> (base64)

(deftype* t (make-instance 'sql-character-large-object-type)
  'base64->object-reader
  'object->base64-writer)

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
  (non-nil-identity-writer type))

;;;;;;;;;;;;;;
;;; Integer-32
;;;
;;; non integer -> (type-error)

(deftype integer-32 ()
  `(integer ,(- (expt 2 31)) ,(1- (expt 2 31))))

(deftype* integer-32 (make-instance 'sql-integer-type :bit-size 32)
  'object->integer-reader
  (non-nil-identity-writer type))

;;;;;;;;;;;;;;
;;; Integer-64
;;;
;;; non integer -> (type-error)

(deftype integer-64 ()
  `(integer ,(- (expt 2 63)) ,(1- (expt 2 63))))

(deftype* integer-64 (make-instance 'sql-integer-type :bit-size 64)
  'object->integer-reader
  (non-nil-identity-writer type))

;;;;;;;;;;;
;;; Integer
;;;
;;; non integer -> (type-error)

(deftype* integer (make-instance 'sql-integer-type)
  'object->integer-reader
  (non-nil-identity-writer type))

;;;;;;;;;;;;
;;; Float-32
;;;
;;; non float -> (type-error)

(deftype float-32 ()
  `float)

(deftype* float-32 (make-instance 'sql-float-type :bit-size 32)
  'object->number-reader
  (non-nil-identity-writer type))

;;;;;;;;;;;;
;;; Float-64
;;;
;;; non float -> (type-error)

(deftype float-64 ()
  `float)

(deftype* float-64 (make-instance 'sql-float-type :bit-size 64)
  'object->number-reader
  (non-nil-identity-writer type))

;;;;;;;;;
;;; Float
;;;
;;; non float -> (type-error)

(deftype* float (make-instance 'sql-float-type)
  'object->number-reader
  (non-nil-identity-writer type))

;;;;;;;;;;
;;; Double
;;;
;;; non double -> (type-error)

(deftype* double (make-instance 'sql-float-type)
  'object->number-reader
  (non-nil-identity-writer type))

;;;;;;;;;;
;;; Number
;;;
;;; non number -> (type-error)

(deftype* number (make-instance 'sql-numeric-type)
  'object->number-reader
  (non-nil-identity-writer type))

;;;;;;;;;;
;;; String
;;;
;;; non string -> (type-error)

(deftype* string (if (consp type-specification)
                     (make-instance 'sql-character-varying-type :size (second type-specification))
                     (make-instance 'sql-character-large-object-type))
  (non-nil-identity-reader type)
  (non-nil-identity-writer type))

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
  (non-nil-identity-reader type)
  (non-nil-identity-writer type))

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
