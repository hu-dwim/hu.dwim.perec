;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;;;;;;;;
;;; Serialized

(defun base64->object-reader (object rdbms-values)
  (declare (ignore object))
  (when-bind rdbms-value (first rdbms-values)
    (with-input-from-sequence (stream
                               (with-input-from-string (base64 rdbms-value)
                                 (decode-base64-bytes base64)))
      (restore stream))))

(defun object->base64-writer (object slot-value)
  (declare (ignore object))
  (list
   (when slot-value
     (with-output-to-string (base64)
                            (encode-base64-bytes
                             (with-output-to-sequence (stream)
                               (store slot-value stream))
                             base64)))))

;;;;;;;;;;;;
;;; Identity

(defun identity-reader (object rdbms-values)
  (declare (ignore object))
  (first rdbms-values))

(defun identity-writer (object slot-value)
  (declare (ignore object))
  (list slot-value))

;;;;;;;;;;
;;; Number

(defun object->number-reader (object rdbms-values)
  (declare (ignore object))
  (when-bind value (first rdbms-values)
    (if (typep value 'number)
        value
        (parse-number value))))

;;;;;;;;;;;
;;; Integer

(defun object->integer-reader (object rdbms-values)
  (declare (ignore object))
  (when-bind value (first rdbms-values)
    (if (typep value 'number)
        value
        (parse-integer value))))

;;;;;;;;;;
;;; Symbol

(defun string->symbol-reader (object rdbms-values)
  (declare (ignore object))
  (symbol-from-canonical-name (first rdbms-values)))

(defun symbol->string-writer (object slot-value)
  (declare (ignore object))
  (list (canonical-symbol-name slot-value)))

;;;;;;;;
;;; List

(defun string->list-reader (object rdbms-values)
  (declare (ignore object))
  (aif (first rdbms-values)
       (read-from-string it)
       it))

(defun list->string-writer (object slot-value)
  (declare (ignore object))
  (list (write-to-string slot-value)))

;;;;;;;;;;;
;;; Boolean

(defun char->boolean-reader (object rdbms-values)
  (declare (ignore object))
  (bind ((value (first rdbms-values)))
    (cond ((eq #\t value) #t)
          ((eq #\f value) #f)
          (t (error "Unknown boolean value")))))

(defun boolean->char-writer (object slot-value)
  (declare (ignore object))
  (if slot-value
      (list #\t)
      (list #\f)))

(defun integer->boolean-reader (object rdbms-values)
  (declare (ignore object))
  (bind ((value (first rdbms-values)))
    (cond ((= 0 value) #t)
          ((= 1 value) #f)
          (t (error "Unknown boolean value")))))

(defun boolean->integer-writer (object slot-value)
  (declare (ignore object))
  (if slot-value
      (list 1)
      (list 0)))

(defun string->boolean-reader (object rdbms-values)
  (declare (ignore object))
  (bind ((value (first rdbms-values)))
    (cond ((equal "t" value) #t)
          ((equal "f" value) #f)
          (t (error "Unknown boolean value")))))

(defun boolean->string-writer (object slot-value)
  (declare (ignore object))
  (if slot-value
      (list "t")
      (list "f")))

(defun object->boolean-reader (object rdbms-values)
  (declare (ignore object))
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
  (lambda (object rdbms-values)
    (declare (ignore object))
    (awhen (first rdbms-values)
      (nth it (slot-definition-type-member-elements type)))))

(defun member->integer-writer (type)
  (lambda (object slot-value)
    (declare (ignore object))
    (list
     (loop for i from 0
           for value in (slot-definition-type-member-elements type)
           when (eq value slot-value)
           do (return i)))))

(defun string->member-reader (type)
  (lambda (object rdbms-values)
    (declare (ignore object))
    (awhen (first rdbms-values)
      (nth it (slot-definition-type-member-elements type)))))

(defun member->string-writer (type)
  (lambda (object slot-value)
    (declare (ignore object))
    (list
     (loop for i from 0
           for value in (slot-definition-type-member-elements type)
           when (eq value slot-value)
           do (return i)))))

;;;;;;;;;;;;;;;;;
;;; Date and time

(defun string->local-time-reader (object rdbms-values)
  (declare (ignore object))
  (awhen (first rdbms-values)
    (let ((*default-timezone* +utc-zone+))
      (parse-timestring it :date-time-separator #\Space))))

(defun local-time->string-writer (object slot-value)
  (declare (ignore object))
  (list
   (when slot-value
     (format-timestring slot-value :date-time-separator #\Space :use-zulu-p #f))))

(defun integer->local-time-reader (object rdbms-values)
  (declare (ignore object))
  (awhen (first rdbms-values)
    (local-time :universal it :timezone +utc-zone+)))

(defun local-time->integer-writer (object slot-value)
  (declare (ignore object))
  (list
   (when slot-value
     (universal-time slot-value))))

;;;;;;;;;;
;;; Object

(defun object-reader (object rdbms-values)
  (declare (ignore object))
  (when (and (first rdbms-values) (second rdbms-values))
    (load-object (make-oid :id (first rdbms-values) :class-name (symbol-from-canonical-name (second rdbms-values)))
                 :skip-existence-check #t)))

(defun object-writer (object slot-value)
  (declare (ignore object))
  (if slot-value
      (oid-values slot-value)
      '(nil nil)))

(defun self-writer (object slot-value)
  (declare (ignore slot-value))
  (object-writer object object))

(defun self-or-nil-writer (object slot-value)
  (if slot-value
      (object-writer object object)
      (object-writer object nil)))
