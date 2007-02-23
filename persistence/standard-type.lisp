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

(defmethod parse-keyword-type-parameters ((type or-type) type-parameters)
  (append (list :types (mapcar #'parse-type (getf type-parameters :types)))
          (call-next-method type (remove-keywords type-parameters :types))))

(defmethod parse-positional-type-parameters ((type or-type) type-parameters)
  (list :types (mapcar #'parse-type type-parameters)))

;;;;;;;
;;; And
;;;
;;; Not supported

(defptype and (&rest types)
  `(and ,@types))

(defmethod parse-keyword-type-parameters ((type and-type) type-parameters)
  (append (list :types (mapcar #'parse-type (getf type-parameters :types)))
          (call-next-method type (remove-keywords type-parameters :types))))

(defmethod parse-positional-type-parameters ((type and-type) type-parameters)
  (list :types (mapcar #'parse-type type-parameters)))

;;;;;;;
;;; Not
;;;
;;; Not supported

(defptype not (negated-type)
  `(not ,negated-type))

(defmethod parse-keyword-type-parameters((type not-type) type-parameters)
  (append (list :negated-type (parse-type (getf type-parameters :negated-type)))
          (call-next-method type (remove-keywords type-parameters :negated-type))))

(defmethod parse-positional-type-parameters ((type not-type) type-parameters)
  (list :negated-type (parse-type (first type-parameters))))

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

(defmapping member (make-instance 'sql-integer-type :bit-size 16)
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

;; this type must be used to mark slots which might be unbound (e.g. (or unbound integer))
(defptype unbound ()
  `(member ,+unbound-slot-value+))

(defmapping unbound (make-instance 'sql-boolean-type)
  (unbound-reader #L(error 'type-error :datum (first !1) :expected-type type))
  (unbound-writer #L(error 'type-error :datum !1 :expected-type type)))

;;;;;;;;
;;; Null
;;; 
;;; nil -> NULL
;;; t -> (type-error)

(defptype null ()
  'null)

(defmapping null (make-instance 'sql-boolean-type)
  (null-reader #L(error 'type-error :datum (first !1) :expected-type type))
  (null-writer #L(error 'type-error :datum !1 :expected-type type)))

;;;;;
;;; t
;;; 
;;; unbound -> NULL, NULL
;;; nil -> true, NULL
;;; other -> true, (base64)

(defptype t ()
  t)

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

(defptype serialized (&optional byte-size)
  (declare (ignore byte-size))
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

(defptype boolean ()
  'boolean)

(defmapping boolean (make-instance 'sql-boolean-type)
  'object->boolean-reader
  'boolean->string-writer)

;;;;;;;;;;;
;;; Integer
;;;
;;; non integer -> (type-error)

(defptype integer (&optional minimum-value maximum-value bit-size)
  (declare (ignore bit-size))
  `(integer ,minimum-value ,maximum-value))

(defmapping integer (make-instance 'sql-integer-type)
  'object->integer-reader
  (non-null-identity-writer type))

;;;;;;;;;;;;;;
;;; Integer-16
;;;
;;; non integer -> (type-error)

(defptype integer-16 ()
  `(integer ,(- (expt 2 15)) ,(1- (expt 2 15))))

(defmapping integer-16 (make-instance 'sql-integer-type :bit-size 16)
  'object->integer-reader
  (non-null-identity-writer type))

;;;;;;;;;;;;;;
;;; Integer-32
;;;
;;; non integer -> (type-error)

(defptype integer-32 ()
  `(integer ,(- (expt 2 31)) ,(1- (expt 2 31))))

(defmapping integer-32 (make-instance 'sql-integer-type :bit-size 32)
  'object->integer-reader
  (non-null-identity-writer type))

;;;;;;;;;;;;;;
;;; Integer-64
;;;
;;; non integer -> (type-error)

(defptype integer-64 ()
  `(integer ,(- (expt 2 63)) ,(1- (expt 2 63))))

(defmapping integer-64 (make-instance 'sql-integer-type :bit-size 64)
  'object->integer-reader
  (non-null-identity-writer type))

;;;;;;;;;
;;; Float
;;;
;;; non float -> (type-error)

(defptype float (&optional minimum-value maximum-value)
  `(float ,minimum-value ,maximum-value))

(defmapping float (make-instance 'sql-float-type :bite-size 64)
  'object->number-reader
  (non-null-identity-writer type))

;;;;;;;;;;;;
;;; Float-32
;;;
;;; non float -> (type-error)

(defptype float-32 ()
  'float)

(defmapping float-32 (make-instance 'sql-float-type :bit-size 32)
  'object->number-reader
  (non-null-identity-writer type))

;;;;;;;;;;;;
;;; Float-64
;;;
;;; non float -> (type-error)

(defptype float-64 ()
  'float)

(defmapping float-64 (make-instance 'sql-float-type :bit-size 64)
  'object->number-reader
  (non-null-identity-writer type))

;;;;;;;;;;
;;; Double
;;;
;;; non double -> (type-error)

(defptype double ()
  'double-float)

(defmapping double-float (make-instance 'sql-float-type :bit-size 64)
  'object->number-reader
  (non-null-identity-writer type))

;;;;;;;;;;
;;; Number
;;;
;;; non number -> (type-error)

(defptype number ()
  'number)

(defmapping number (make-instance 'sql-numeric-type)
  'object->number-reader
  (non-null-identity-writer type))

;;;;;;;;;;
;;; String
;;;
;;; non string -> (type-error)

(defptype string (&optional length acceptable-characters)
  (declare (ignore acceptable-characters))
  `(string ,length))

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

(defptype text (&optional maximum-length acceptable-characters)
  (declare (ignore maximum-length acceptable-characters))
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

(defptype symbol ()
  'symbol)

(defmapping symbol (make-instance 'sql-character-large-object-type)
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

(defptype date ()
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

(defptype time ()
  '(and local-time
        (satisfies time-p)))

(defmapping time (make-instance 'sql-time-type)
  'string->local-time-reader
  'local-time->string-writer)

;;;;;;;;;;;;;
;;; Timestamp
;;;
;;; non date -> (type-error)

(defptype timestamp ()
  '(and local-time
        (satisfies date-p)
        (satisfies time-p)))

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

(defptype duration ()
  '(and string
        (satisfies duration-p)))

(defmapping duration (make-instance 'sql-character-varying-type :size 32)
  (non-null-identity-reader type)
  (non-null-identity-writer type))

;;;;;;;;
;;; Form
;;;
;;; non form -> (type-error)

(defptype form (&optional byte-size)
  (declare (ignore byte-size))
  '(and list
        (satisfies maximum-serialized-size-p)))

(defmapping form (make-instance 'sql-character-varying-type)
  'string->list-reader
  'list->string-writer)
