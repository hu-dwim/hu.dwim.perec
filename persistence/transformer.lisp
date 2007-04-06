;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;;;;;
;;; Unbound

(defun equaln (value-1 value-2 count)
  (iter (repeat count)
        (for v-1 in value-1)
        (for v-2 in value-2)
        (unless (eq v-1 v-2)
          (return-from equaln #f)))
  #t)

(defcondition* slot-type-error (type-error)
  ((slot
    :type persistent-effective-slot-definition))
  (:report
   (lambda (condition stream)
     (format stream
             "~@<The value ~2I~:_~S ~I~_in slot ~A is not of type ~2I~_~S.~:>"
             (type-error-datum condition)
             (slot-definition-name (slot-of condition))
             (type-error-expected-type condition)))))

(defmacro def-transformer-wrapper (name &body forms)
  `(defun ,name (slot type function column-number)
    (declare (ignorable slot type column-number))
    ,@forms))

(def-transformer-wrapper unbound-reader
  (bind ((unbound-rdbms-value (iter (repeat column-number) (collect nil))))
    (lambda (rdbms-values)
      (if (equaln unbound-rdbms-value rdbms-values column-number)
          +unbound-slot-value+
          (funcall function rdbms-values)))))

(def-transformer-wrapper non-unbound-reader
  (lambda (rdbms-values)
    (prog1-bind slot-value (funcall function rdbms-values)
      (when (eq +unbound-slot-value+ slot-value)
        (if slot
            (error 'unbound-slot :instance nil :name (slot-definition-name slot))
            (error 'type-error :datum slot-value :expected-type type))))))

(def-transformer-wrapper unbound-writer
  (bind ((unbound-rdbms-value (iter (repeat column-number) (collect nil))))
    (lambda (slot-value)
      (if (eq +unbound-slot-value+ slot-value)
          unbound-rdbms-value
          (funcall function slot-value)))))

(def-transformer-wrapper non-unbound-writer
  (lambda (slot-value)
    (if (eq +unbound-slot-value+ slot-value)
        (if slot
            (error 'unbound-slot :instance nil :name (slot-definition-name slot))
            (error 'type-error :datum slot-value :expected-type type))
        (funcall function slot-value))))

;;;;;;;;
;;; Null

(def-transformer-wrapper null-reader
  (bind ((nil-rdbms-value (iter (repeat column-number) (collect nil))))
    (lambda (rdbms-values)
      (if (equaln nil-rdbms-value rdbms-values column-number)
          nil
          (funcall function rdbms-values)))))

(def-transformer-wrapper non-null-reader
  (lambda (rdbms-values)
    (prog1-bind slot-value (funcall function rdbms-values)
      (unless slot-value
        (if slot
            (error 'slot-type-error :slot slot :datum slot-value :expected-type type)
            (error 'type-error :datum slot-value :expected-type type))))))

(def-transformer-wrapper null-writer
  (bind ((nil-rdbms-value (iter (repeat column-number) (collect nil))))
    (lambda (slot-value)
      (if slot-value
          (funcall function slot-value)
          nil-rdbms-value))))

(def-transformer-wrapper non-null-writer
  (lambda (slot-value)
    (if slot-value
        (funcall function slot-value)
        (if slot
            (error 'slot-type-error :slot slot :datum slot-value :expected-type type)
            (error 'type-error :datum slot-value :expected-type type)))))

;;;;;;;;;;;;;;;;;;;
;;; Unbound or null

(def-transformer-wrapper unbound-or-null-reader
  (bind ((unbound-rdbms-value (iter (repeat column-number) (collect nil)))
         (nil-rdbms-value (list* #t (cdr unbound-rdbms-value))))
    (lambda (rdbms-values)
      (cond ((equaln unbound-rdbms-value rdbms-values column-number)
             +unbound-slot-value+)
            ((equaln nil-rdbms-value rdbms-values column-number)
             nil)
            (t (funcall function (cdr rdbms-values)))))))

(def-transformer-wrapper unbound-or-null-writer
  (bind ((unbound-rdbms-value (iter (repeat column-number) (collect nil)))
         (nil-rdbms-value (list* #t (cdr unbound-rdbms-value))))
    (lambda (slot-value)
      (cond ((eq +unbound-slot-value+ slot-value)
             unbound-rdbms-value)
            ((null slot-value)
             nil-rdbms-value)
            (t (list* #t (funcall function slot-value)))))))

;;;;;;;;;;;;;;
;;; Serialized

(defun base64->object-reader (rdbms-values)
  (with-input-from-sequence (stream
    (with-input-from-string (base64 (first rdbms-values))
      (decode-base64-bytes base64)))
    (restore stream)))

(defun object->base64-writer (slot-value)
  (list
   (with-output-to-string (base64)
     (encode-base64-bytes
      (with-output-to-sequence (stream)
        (store slot-value stream))
      base64))))

;;;;;;;;;;;;
;;; Identity

(defun identity-reader (rdbms-values)
  (first rdbms-values))

(defun identity-writer (slot-value)
  (list slot-value))

;;;;;;;;;;
;;; Number

(defun object->number-reader (rdbms-values)
  (bind ((value (first rdbms-values)))
    (if (typep value 'number)
        value
        (parse-number value))))

;;;;;;;;;;;
;;; Integer

(defun object->integer-reader (rdbms-values)
  (bind ((value (first rdbms-values)))
    (if (typep value 'number)
        value
        (parse-integer value))))

;;;;;;;;;;
;;; Symbol

(defun string->symbol-reader (rdbms-values)
  (symbol-from-canonical-name (first rdbms-values)))

(defun symbol->string-writer (slot-value)
  (list (canonical-symbol-name slot-value)))

;;;;;;;;
;;; List

(defun string->list-reader (rdbms-values)
  (read-from-string (first rdbms-values)))

(defun list->string-writer (slot-value)
  (list (write-to-string slot-value)))

;;;;;;;;;;;
;;; Boolean

(defun char->boolean-reader (rdbms-values)
  (bind ((value (first rdbms-values)))
    (cond ((eq #\t value) #t)
          ((eq #\f value) #f)
          (t (error 'type-error :datum value :expected-type 'boolean)))))

(defun boolean->char-writer (slot-value)
  (if slot-value
      (list #\t)
      (list #\f)))

(defun integer->boolean-reader (rdbms-values)
  (bind ((value (first rdbms-values)))
    (cond ((= 0 value) #t)
          ((= 1 value) #f)
          (t (error 'type-error :datum value :expected-type 'boolean)))))

(defun boolean->integer-writer (slot-value)
  (if slot-value
      (list 1)
      (list 0)))

(defun string->boolean-reader (rdbms-values)
  (bind ((value (first rdbms-values)))
    (cond ((equal "t" value) #t)
          ((equal "f" value) #f)
          (t (error 'type-error :datum value :expected-type 'boolean)))))

(defun boolean->string-writer (slot-value)
  (if slot-value
      (list "TRUE")
      (list "FALSE")))

(defun object->boolean-reader (rdbms-values)
  (bind ((value (first rdbms-values)))
    (cond ((eq #t value) #t)
          ((eq #f value) #f)
          ((eq #\t value) #t)
          ((eq #\f value) #f)
          ((and (typep value 'integer)
                (= 0 value)) #f)
          ((and (typep value 'integer)
                (= 1 value)) #t)
          ((equal "t" value) #t)
          ((equal "f" value) #f)
          ((equal "TRUE" value) #t)
          ((equal "FALSE" value) #f)
          (t (error 'type-error :datum value :expected-type 'boolean)))))

;;;;;;;;;;
;;; Member

(defun slot-definition-type-member-elements (type)
  (cdr (if (eq 'member (first type))
           type
           (find 'member type
                 :key #L(when (listp !1)
                          (first !1))))))

(defun integer->member-reader (type)
  (bind ((member-elements (slot-definition-type-member-elements type)))
    (lambda (rdbms-values)
      (bind ((value (first rdbms-values)))
        (aif (nth value member-elements)
             it
             (error 'type-error :datum value :expected-type type))))))

(defun member->integer-writer (type)
  (bind ((member-elements (slot-definition-type-member-elements type)))
    (lambda (slot-value)
      (block found
        (loop for i from 0
              for value in member-elements
              when (eq value slot-value)
              do (return-from found (list i)))
        (error 'type-error :datum slot-value :expected-type type)))))

(defun string->member-reader (type)
  (bind ((member-elements (slot-definition-type-member-elements type)))
    (lambda (rdbms-values)
      (aprog1 (string->symbol-reader rdbms-values)
        (assert (member it member-elements))))))

(defun member->string-writer (type)
  (bind ((member-elements (slot-definition-type-member-elements type)))
    (lambda (slot-value)
      (assert (member slot-value member-elements))
      (symbol->string-writer slot-value))))

;;;;;;;;;;;;;;;;;
;;; Date and time

(defun string->local-time-reader (rdbms-values)
  (bind ((*default-timezone* +utc-zone+))
    (parse-timestring (first rdbms-values) :date-time-separator #\Space)))

(defun integer->local-time-reader (rdbms-values)
  ;; NOTE: assuming that the database server is configured to UTC time zone
  (local-time :universal (first rdbms-values) :timezone +utc-zone+))

(defun date->string-writer (slot-value)
  (assert (eq (timezone-of slot-value) +utc-zone+))
  (list
   (format-timestring slot-value :omit-time-part-p #t)))

(defun time->string-writer (slot-value)
  (list
   (format-timestring slot-value :omit-date-part-p #t :omit-timezone-part-p #t)))

(defun timestamp->string-writer (slot-value)
  (list
   (format-timestring slot-value :date-time-separator #\Space :use-zulu-p #f)))

(defun local-time->integer-writer (slot-value)
  (list
   (universal-time slot-value)))

;;;;;;;;;;
;;; Object

(defun object-reader (rdbms-values)
  (load-instance (make-oid :id (first rdbms-values) :class-name (symbol-from-canonical-name (second rdbms-values)))
                 :skip-existence-check #t))

(defun object-writer (slot-value)
  (oid-values slot-value))
