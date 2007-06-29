;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;
;;; Or
;;;
;;; May be used to combine null and unbound with a primitive type and may generate an extra column.
;;; See compute-reader and compute-writer generic function definitions

(defptype or (&rest types)
  `(or ,@types))

(defmethod shared-initialize :around ((type or-type) slot-names &rest args &key types &allow-other-keys)
  (apply #'call-next-method type slot-names :types (mapcar #'parse-type types) (remove-keywords args :types)))

;;;;;;;
;;; And
;;;
;;; Not supported

(defptype and (&rest types)
  `(and ,@types))

(defmethod shared-initialize :around ((type and-type) slot-names &rest args &key types &allow-other-keys)
  (apply #'call-next-method type slot-names :types (mapcar #'parse-type types) (remove-keywords args :types)))

;;;;;;;
;;; Not
;;;
;;; Not supported

(defptype not (negated-type)
  `(not ,negated-type))

(defmethod shared-initialize :around ((type not-type) slot-names &rest args &key negated-type &allow-other-keys)
  (apply #'call-next-method type slot-names :negated-type (parse-type negated-type) (remove-keywords args :negated-type)))

;;;;;;;;;;;;;
;;; Satisfies

(defptype satisfies (function)
  `(satisfies ,function))

;;;;;;;
;;; Nil
;;;
;;; other -> (type-error)

(defptype nil ()
  nil)

(defmapping nil nil
  #L(error 'type-error :datum (first !1) :expected-type nil)
  #L(error 'type-error :datum !1 :expected-type nil))

;;;;;;;;;;
;;; Member
;;;
;;; not found in members -> (type-error)

(defptype member (&rest members)
  `(member ,@members))

(defmapping member (sql-integer-type :bit-size 16)
  (integer->member-reader type-specification)
  (member->integer-writer type-specification))

(defmacro def-member-type (name &body members)
  `(defptype ,name ()
    `(member ,@',members)))

;;;;;;;;;;;
;;; Unbound
;;; 
;;; unbound -> NULL
;;; t -> type-error

(defptype eql (value)
  `(eql ,value))

;; this type must be used to mark slots which might be unbound (e.g. (or unbound integer))
(defptype unbound ()
  `(eql ,+unbound-slot-value+))

(defmapping unbound (sql-boolean-type)
  (unbound-reader #L(error 'type-error :datum (first !1) :expected-type type))
  (unbound-writer #L(error 'type-error :datum !1 :expected-type type)))

;;;;;;;;
;;; Null
;;; 
;;; nil -> NULL
;;; t -> (type-error)

(defptype null ()
  'null)

(defmapping null (sql-boolean-type)
  (null-reader #L(error 'type-error :datum (first !1) :expected-type type))
  (null-writer #L(error 'type-error :datum !1 :expected-type type)))

;;;;;
;;; t
;;; 
;;; unbound -> NULL, NULL
;;; nil -> true, NULL
;;; other -> true, (byte-vector)

(defptype t ()
  t)

(defmapping t (sql-binary-large-object-type)
  'byte-vector->object-reader
  'object->byte-vector-writer)

;;;;;;;;;;;;;;
;;; Serialized
;;; 
;;; unbound -> (type-error)
;;; nil -> (type-error)
;;; other -> (byte-vector)

(defun maximum-serialized-size-p (serialized)
  (declare (ignore serialized))
  t)

(defptype serialized (&optional byte-size)
  (declare (ignore byte-size))
  '(and (not unbound)
        (not null)
        (satisfies maximum-serialized-size-p)))

(defmapping serialized (if (consp type-specification)
                           (sql-binary-large-object-type :size (byte-size-of (parse-type type-specification)))
                           (sql-binary-large-object-type))
  'byte-vector->object-reader
  'object->byte-vector-writer)

;;;;;;;;;;;
;;; Boolean
;;; 
;;; nil -> false
;;; t -> true
;;; other -> (type-error)

(defptype boolean ()
  'boolean)

(defmapping boolean (sql-boolean-type)
  'object->boolean-reader
  'boolean->string-writer)

;;;;;;;;;;;
;;; Integer
;;;
;;; non integer -> (type-error)

(defptype integer (&optional minimum-value maximum-value bit-size)
  (declare (ignore bit-size))
  `(integer ,minimum-value ,maximum-value))

(defmapping integer (sql-integer-type)
  'object->integer-reader
  'identity-writer)

;;;;;;;;;;;;;;
;;; Integer-16
;;;
;;; non integer -> (type-error)

(defptype integer-16 ()
  `(integer ,(- (expt 2 15)) ,(1- (expt 2 15))))

(defmapping integer-16 (sql-integer-type :bit-size 16)
  'object->integer-reader
  'identity-writer)

;;;;;;;;;;;;;;
;;; Integer-32
;;;
;;; non integer -> (type-error)

(defptype integer-32 ()
  `(integer ,(- (expt 2 31)) ,(1- (expt 2 31))))

(defmapping integer-32 (sql-integer-type :bit-size 32)
  'object->integer-reader
  'identity-writer)

;;;;;;;;;;;;;;
;;; Integer-64
;;;
;;; non integer -> (type-error)

(defptype integer-64 ()
  `(integer ,(- (expt 2 63)) ,(1- (expt 2 63))))

(defmapping integer-64 (sql-integer-type :bit-size 64)
  'object->integer-reader
  'identity-writer)

;;;;;;;;;
;;; Float
;;;
;;; non float -> (type-error)

(defptype float (&optional minimum-value maximum-value)
  `(float ,minimum-value ,maximum-value))

(defmapping float (sql-float-type :bite-size 64)
  'object->number-reader
  'identity-writer)

;;;;;;;;;;;;
;;; Float-32
;;;
;;; non float -> (type-error)

;; TODO: minimum-value maximum-value
(defptype float-32 ()
  'float)

(defmapping float-32 (sql-float-type :bit-size 32)
  'object->number-reader
  'identity-writer)

;;;;;;;;;;;;
;;; Float-64
;;;
;;; non float -> (type-error)

;; TODO: minimum-value maximum-value
(defptype float-64 ()
  'float)

(defmapping float-64 (sql-float-type :bit-size 64)
  'object->number-reader
  'identity-writer)

;;;;;;;;;;
;;; Double
;;;
;;; non double -> (type-error)

(defptype double ()
  'double-float)

(defmapping double-float (sql-float-type :bit-size 64)
  'object->number-reader
  'identity-writer)

;;;;;;;;;;
;;; Number
;;;
;;; non number -> (type-error)

(defptype number ()
  'number)

(defmapping number (sql-numeric-type)
  'object->number-reader
  'identity-writer)

;;;;;;;;;;
;;; String
;;;
;;; non string -> (type-error)

(defptype string (&optional length acceptable-characters)
  (declare (ignore acceptable-characters))
  `(string ,length))

(defmapping string (if (consp type-specification)
                       (sql-character-type :size (length-of (parse-type type-specification)))
                       (sql-character-large-object-type))
  'identity-reader
  'identity-writer)

;;;;;;;;
;;; Text
;;;
;;; non string -> (type-error)

;; TODO:
(defun maximum-length-p (string)
  (declare (ignore string))
  t)

(defptype text (&optional maximum-length minimum-length acceptable-characters)
  (declare (ignore maximum-length minimum-length acceptable-characters))
  '(and string
        (satisfies maximum-length-p)))

(defmapping text (if (consp type-specification)
                     (sql-character-varying-type :size (maximum-length-of (parse-type type-specification)))
                     (sql-character-large-object-type))
  'identity-reader
  'identity-writer)

;;;;;;;;;;
;;; Symbol
;;;
;;; non symbol -> (type-error)

(defptype symbol ()
  'symbol)

(defmapping symbol (sql-character-large-object-type)
  'string->symbol-reader
  'symbol->string-writer)

;; TODO:
(defun maximum-symbol-name-length-p (symbol)
  (declare (ignore symbol))
  t)

(defptype symbol* (&optional maximum-length)
  (declare (ignore maximum-length))
  '(and symbol
        (satisfies maximum-symbol-name-length-p)))

(defmapping symbol* (if (consp type-specification)
                        (sql-character-varying-type :size (second (find 'symbol* type-specification
                                                                        :key #L(when (listp !1) (first !1)))))
                        (sql-character-large-object-type))
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

(defptype date ()
  '(and local-time
        (satisfies date-p)))

(defmapping date (sql-date-type)
  'identity-reader
  'date->string-writer)

;;;;;;;;
;;; Time
;;;
;;; non date -> (type-error)

;; TODO:
(defun time-p (time)
  (declare (ignore time))
  t)

(defptype time ()
  '(and local-time
        (satisfies time-p)))

(defmapping time (sql-time-type)
  'identity-reader
  'time->string-writer)

;;;;;;;;;;;;;
;;; Timestamp
;;;
;;; non date -> (type-error)

(defptype timestamp ()
  '(and local-time
        (satisfies date-p)
        (satisfies time-p)))

(defmapping timestamp (sql-timestamp-type :with-timezone #t)
  'identity-reader
  'timestamp->string-writer)

;;;;;;;;;;;;
;;; Duration
;;;
;;; non string -> (type-error)

;; TODO:
(defun duration-p (duration)
  (declare (ignore duration))
  t)

(defptype duration ()
  '(and string
        (satisfies duration-p)))

(defmapping duration (sql-character-varying-type :size 32)
  'identity-reader
  'identity-writer)

;;;;;;;;
;;; List
;;;
;;; non form -> (type-error)

(defptype list (&optional byte-size)
  (declare (ignore byte-size))
  'list)

(defmapping list (sql-character-varying-type)
  'string->list-reader
  'list->string-writer)

;;;;;;;;
;;; Form
;;;
;;; non form -> (type-error)

(defptype form (&optional byte-size)
  (declare (ignore byte-size))
  '(and list
        (satisfies maximum-serialized-size-p)))

(defmapping form (sql-character-varying-type)
  'string->list-reader
  'list->string-writer)

;;;;;;;;;;;;;;;;;
;;;
;;; Unsigned byte

(defptype unsigned-byte (&optional size)
  `(unsigned-byte ,size))

;;;;;;;;;;
;;; Vector

(defptype vector (&optional (element-type '*) (size '*))
  `(vector ,element-type ,size))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unsigned byte vector

;; TODO assert for the size in the reader/writer
(defptype unsigned-byte-vector (&optional size)
  `(vector (unsigned-byte 8) ,size))

(defmapping unsigned-byte-vector (sql-binary-large-object-type)
  'identity-reader
  'identity-writer)
