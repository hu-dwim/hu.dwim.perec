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

;; this type must be used to mark slots which might be unbound (e.g. (or unbound null integer))
(deftype unbound ()
  nil)

;;;;;;;;;;;
;;; Boolean

(deftype* boolean (make-instance 'sql-boolean-type)
  'object->boolean-reader
  'identity-writer)

;;;;;;;;;;;;;;
;;; Integer-16

(deftype integer-16 ()
  `(integer ,(- (expt 2 15)) ,(1- (expt 2 15))))

(deftype* integer-16 (make-instance 'sql-integer-type :bit-size 16)
  'object->integer-reader
  'identity-writer)

;;;;;;;;;;;;;;
;;; Integer-32

(deftype integer-32 ()
  `(integer ,(- (expt 2 31)) ,(1- (expt 2 31))))

(deftype* integer-32 (make-instance 'sql-integer-type :bit-size 32)
  'object->integer-reader
  'identity-writer)

;;;;;;;;;;;;;;
;;; Integer-64

(deftype integer-64 ()
  `(integer ,(- (expt 2 63)) ,(1- (expt 2 63))))

(deftype* integer-64 (make-instance 'sql-integer-type :bit-size 64)
  'object->integer-reader
  'identity-writer)

;;;;;;;;;;;
;;; Integer

(deftype* integer (make-instance 'sql-integer-type)
  'object->integer-reader
  'identity-writer)

;;;;;;;;;;;;
;;; Float-32

(deftype float-32 ()
  `float)

(deftype* float-32 (make-instance 'sql-float-type :bit-size 32)
  'object->number-reader
  'identity-writer)

;;;;;;;;;;;;
;;; Float-64

(deftype float-64 ()
  `float)

(deftype* float-64 (make-instance 'sql-float-type :bit-size 64)
  'object->number-reader
  'identity-writer)

;;;;;;;;;
;;; Float

(deftype* float (make-instance 'sql-float-type)
  'object->number-reader
  'identity-writer)

;;;;;;;;;;
;;; Number

(deftype* number (make-instance 'sql-numeric-type)
  'object->number-reader
  'identity-writer)

;;;;;;;;;;
;;; String

(deftype* string (if (consp type-specification)
                     (make-instance 'sql-character-varying-type :size (second type-specification))
                     (make-instance 'sql-character-large-object-type))
  'identity-reader
  'identity-writer)

;;;;;;;;;;
;;; Symbol

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

;;;;;
;;; t

(deftype* t (make-instance 'sql-character-large-object-type)
  'base64->object-reader
  'object->base64-writer)

;;;;;;;;;;;;;;
;;; Serialized

(deftype serialized (&optional size)
  (declare (ignore size))
  t)

(deftype* serialized (if (consp type-specification)
                         (make-instance 'sql-character-varying-type :size (second type-specification))
                         (make-instance 'sql-character-large-object-type))
  'base64->object-reader
  'object->base64-writer)

;;;;;;;;
;;; Date

(deftype date ()
  'local-time)

(deftype* date (make-instance 'sql-date-type)
  'integer->local-time-reader
  'local-time->string-writer)

;;;;;;;;
;;; Time

(deftype time ()
  'local-time)

(deftype* time (make-instance 'sql-time-type)
  'string->local-time-reader
  'local-time->string-writer)

;;;;;;;;;;;;;
;;; Timestamp

(deftype timestamp ()
  'local-time)

(deftype* timestamp (make-instance 'sql-timestamp-type)
  'integer->local-time-reader
  'local-time->string-writer)

;;;;;;;;;;;;
;;; Duration

(deftype duration ()
  'string)

(deftype* duration (make-instance 'sql-character-varying-type :size 32)
  'identity-reader
  'identity-writer)

;;;;;;;;
;;; Form

(deftype form (&optional size)
  (declare (ignore size))
  'list)

(deftype* form (make-instance 'sql-character-varying-type)
  'string->list-reader
  'list->string-writer)

;;;;;;;;;;
;;; Member

(deftype* member (make-instance 'sql-integer-type :bit-size 16)
  (integer->member-reader type-specification)
  (member->integer-writer type-specification))
