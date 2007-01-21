;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;;;;;;;;
;;; Serialized

(defun base64->object-reader (object slot rdbms-values)
  (declare (ignore object slot))
  (when-bind rdbms-value (first rdbms-values)
    (with-input-from-sequence (stream
                               (with-input-from-string (base64 rdbms-value)
                                 (decode-base64-bytes base64)))
      (restore stream))))

(defun object->base64-writer (object slot slot-value)
  (declare (ignore object slot))
  (list
   (when slot-value
     (with-output-to-string (base64)
                            (encode-base64-bytes
                             (with-output-to-sequence (stream)
                               (store slot-value stream))
                             base64)))))

;;;;;;;;;;;;
;;; Identity

(defun identity-reader (object slot rdbms-values)
  (declare (ignore object slot))
  (first rdbms-values))

(defun identity-writer (object slot slot-value)
  (declare (ignore object slot))
  (list slot-value))

;;;;;;;;;;
;;; Number

(defun object->number-reader (object slot rdbms-values)
  (declare (ignore object slot))
  (when-bind value (first rdbms-values)
    (if (typep value 'number)
        value
        (parse-number value))))

;;;;;;;;;;;
;;; Integer

(defun object->integer-reader (object slot rdbms-values)
  (declare (ignore object slot))
  (when-bind value (first rdbms-values)
    (if (typep value 'number)
        value
        (parse-integer value))))

;;;;;;;;;;
;;; Symbol

(defun string->symbol-reader (object slot rdbms-values)
  (declare (ignore object slot))
  (symbol-from-canonical-name (first rdbms-values)))

(defun symbol->string-writer (object slot slot-value)
  (declare (ignore object slot))
  (list (canonical-symbol-name slot-value)))

;;;;;;;;
;;; List

(defun string->list-reader (object slot rdbms-values)
  (declare (ignore object slot))
  (aif (first rdbms-values)
       (read-from-string it)
       it))

(defun list->string-writer (object slot slot-value)
  (declare (ignore object slot))
  (list (write-to-string slot-value)))

;;;;;;;;;;;
;;; Boolean

(defun char->boolean-reader (object slot rdbms-values)
  (declare (ignore object slot))
  (bind ((value (first rdbms-values)))
    (cond ((eq #\t value) #t)
          ((eq #\f value) #f)
          (t (error "Unknown boolean value")))))

(defun boolean->char-writer (object slot slot-value)
  (declare (ignore object slot))
  (if slot-value
      (list #\t)
      (list #\f)))

(defun integer->boolean-reader (object slot rdbms-values)
  (declare (ignore object slot))
  (bind ((value (first rdbms-values)))
    (cond ((= 0 value) #t)
          ((= 1 value) #f)
          (t (error "Unknown boolean value")))))

(defun boolean->integer-writer (object slot slot-value)
  (declare (ignore object slot))
  (if slot-value
      (list 1)
      (list 0)))

(defun string->boolean-reader (object slot rdbms-values)
  (declare (ignore object slot))
  (bind ((value (first rdbms-values)))
    (cond ((equal "t" value) #t)
          ((equal "f" value) #f)
          (t (error "Unknown boolean value")))))

(defun boolean->string-writer (object slot slot-value)
  (declare (ignore object slot))
  (if slot-value
      (list "t")
      (list "f")))

(defun object->boolean-reader (object slot rdbms-values)
  (declare (ignore object slot))
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

;;;;;;;;;;;;;;
;;; Enumerated

(defun slot-definition-type-member-elements (slot)
  (let ((type (slot-definition-type slot)))
    (cdr (if (eq 'member (first type))
             type
             (find 'member type
                   :key #L(when (listp !1)
                            (first !1)))))))

(defun integer->enumerated-reader (object slot rdbms-values)
  (declare (ignore object))
  (awhen (first rdbms-values)
    (nth it (slot-definition-type-member-elements slot))))

(defun enumerated->integer-writer (object slot slot-value)
  (declare (ignore object))
  (list
   (loop for i from 0
         for value in (slot-definition-type-member-elements slot)
         when (eq value slot-value)
         do (return i))))

(defun string->enumerated-reader (object slot rdbms-values)
  (declare (ignore object))
  (awhen (first rdbms-values)
    (nth it (slot-definition-type-member-elements slot))))

(defun enumerated->string-writer (object slot slot-value)
  (declare (ignore object))
  (list
   (loop for i from 0
         for value in (slot-definition-type-member-elements slot)
         when (eq value slot-value)
         do (return i))))

;;;;;;;;;;;;;;;;;
;;; Date and time

(defun string->local-time-reader (object slot rdbms-values)
  (declare (ignore object slot))
  (awhen (first rdbms-values)
    (let ((*default-timezone* +utc-zone+))
      (parse-timestring it :date-time-separator #\Space))))

(defun local-time->string-writer (object slot slot-value)
  (declare (ignore object slot))
  (list
   (when slot-value
     (format-timestring slot-value :date-time-separator #\Space :use-zulu-p #f))))

(defun integer->local-time-reader (object slot rdbms-values)
  (declare (ignore object slot))
  (awhen (first rdbms-values)
    (local-time :universal it :timezone +utc-zone+)))

(defun local-time->integer-writer (object slot slot-value)
  (declare (ignore object slot))
  (list
   (when slot-value
     (universal-time slot-value))))

;;;;;;;;;;
;;; Object

(defun object-reader (object slot rdbms-values)
  (declare (ignore object slot))
  (when (and (first rdbms-values) (second rdbms-values))
    (load-object (make-oid :id (first rdbms-values) :class-name (symbol-from-canonical-name (second rdbms-values)))
                 :skip-existence-check #t)))

(defun object-writer (object slot slot-value)
  (declare (ignore object slot))
  (if slot-value
      (oid-values slot-value)
      '(nil nil)))

(defun self-writer (object slot slot-value)
  (declare (ignore slot-value))
  (object-writer object slot object))

(defun self-or-nil-writer (object slot slot-value)
  (if slot-value
      (object-writer object slot object)
      (object-writer object slot nil)))
