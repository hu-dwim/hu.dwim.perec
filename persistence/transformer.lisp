;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;;;;;
;;; Unbound

(defun unbound-reader (function)
  (lambda (rdbms-values)
    (aif (first rdbms-values)
         (funcall function it)
         +the-unbound-slot-value+)))

(defun unbound-writer (function)
  (lambda (slot-value)
    (if (eq +the-unbound-slot-value+ slot-value)
        '(nil)
        (funcall function it))))

;;;;;;;;
;;; Null

(defun null-reader (function)
  (lambda (rdbms-values)
    (awhen (first rdbms-values)
      (funcall function it))))

(defun null-writer (function)
  (lambda (rdbms-values)
    (awhen (first rdbms-values)
      (funcall function it))))

;;;;;;;;;;;;;;
;;; Serialized

(defun base64->object-reader (rdbms-values)
  (when-bind rdbms-value (first rdbms-values)
    (with-input-from-sequence (stream
                               (with-input-from-string (base64 rdbms-value)
                                 (decode-base64-bytes base64)))
      (restore stream))))

(defun object->base64-writer (slot-value)
  (list
   (when slot-value
     (with-output-to-string (base64)
       (encode-base64-bytes
        (with-output-to-sequence (stream)
          (store slot-value stream))
        base64)))))

;;;;;;;;;;;;
;;; Identity

(defun identity-reader (rdbms-values)
  (first rdbms-values))

(defun identity-writer (slot-value)
  (list slot-value))

;;;;;;;;;;
;;; Number

(defun object->number-reader (rdbms-values)
  (when-bind value (first rdbms-values)
    (if (typep value 'number)
        value
        (parse-number value))))

;;;;;;;;;;;
;;; Integer

(defun object->integer-reader (rdbms-values)
  (when-bind value (first rdbms-values)
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
  (aif (first rdbms-values)
       (read-from-string it)
       it))

(defun list->string-writer (slot-value)
  (list (write-to-string slot-value)))

;;;;;;;;;;;
;;; Boolean

(defun char->boolean-reader (rdbms-values)
  (bind ((value (first rdbms-values)))
    (cond ((eq #\t value) #t)
          ((eq #\f value) #f)
          (t (error "Unknown boolean value")))))

(defun boolean->char-writer (slot-value)
  (if slot-value
      (list #\t)
      (list #\f)))

(defun integer->boolean-reader (rdbms-values)
  (bind ((value (first rdbms-values)))
    (cond ((= 0 value) #t)
          ((= 1 value) #f)
          (t (error "Unknown boolean value")))))

(defun boolean->integer-writer (slot-value)
  (if slot-value
      (list 1)
      (list 0)))

(defun string->boolean-reader (rdbms-values)
  (bind ((value (first rdbms-values)))
    (cond ((equal "t" value) #t)
          ((equal "f" value) #f)
          (t (error "Unknown boolean value")))))

(defun boolean->string-writer (slot-value)
  (if slot-value
      (list "t")
      (list "f")))

(defun object->boolean-reader (rdbms-values)
  (bind ((value (first rdbms-values)))
    (cond ((eq #t value) #t)
          ((eq #f value) #f)
          ((eq #\t value) #t)
          ((eq #\f value) #f)
          ((and (typep value 'integer)
                (= 0 value)) #t)
          ((and (typep value 'integer)
                (= 1 value)) #f)
          ((equal "t" value) #t)
          ((equal "f" value) #f)
          (t (error "Unknown boolean value")))))

;;;;;;;;;;
;;; Member

(defun slot-definition-type-member-elements (type)
  (cdr (if (eq 'member (first type))
           type
           (find 'member type
                 :key #L(when (listp !1)
                          (first !1))))))

(defun integer->member-reader (type)
  (lambda (rdbms-values)
    (awhen (first rdbms-values)
      (nth it (slot-definition-type-member-elements type)))))

(defun member->integer-writer (type)
  (lambda (slot-value)
    (list
     (loop for i from 0
           for value in (slot-definition-type-member-elements type)
           when (eq value slot-value)
           do (return i)))))

(defun string->member-reader (type)
  (lambda (rdbms-values)
    (awhen (first rdbms-values)
      (nth it (slot-definition-type-member-elements type)))))

(defun member->string-writer (type)
  (lambda (slot-value)
    (list
     (loop for i from 0
           for value in (slot-definition-type-member-elements type)
           when (eq value slot-value)
           do (return i)))))

;;;;;;;;;;;;;;;;;
;;; Date and time

(defun string->local-time-reader (rdbms-values)
  (awhen (first rdbms-values)
    (let ((*default-timezone* +utc-zone+))
      (parse-timestring it :date-time-separator #\Space))))

(defun local-time->string-writer (slot-value)
  (list
   (when slot-value
     (format-timestring slot-value :date-time-separator #\Space :use-zulu-p #f))))

(defun integer->local-time-reader (rdbms-values)
  (awhen (first rdbms-values)
    (local-time :universal it :timezone +utc-zone+)))

(defun local-time->integer-writer (slot-value)
  (list
   (when slot-value
     (universal-time slot-value))))

;;;;;;;;;;
;;; Object

(defun object-reader (rdbms-values)
  (when (and (first rdbms-values) (second rdbms-values))
    (load-object (make-oid :id (first rdbms-values) :class-name (symbol-from-canonical-name (second rdbms-values)))
                 :skip-existence-check #t)))

(defun object-writer (slot-value)
  (if slot-value
      (oid-values slot-value)
      '(nil nil)))
