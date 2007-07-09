;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;;;;;
;;; Unbound

(defun is-vector-of-constant (vector value index length)
  (declare (type (or null fixnum) length)
           (type fixnum index)
           (optimize (speed 1)))
  (iter (for i :from index)
        (repeat length)
        (always (eql (elt vector i) value))))

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
  (lambda (rdbms-values index)
    (if (is-vector-of-constant rdbms-values :null index column-number)
        +unbound-slot-value+
        (funcall function rdbms-values index))))

(def-transformer-wrapper non-unbound-reader
  (lambda (rdbms-values index)
    (prog1-bind slot-value (funcall function rdbms-values index)
      (when (eq +unbound-slot-value+ slot-value)
        (if slot
            (error 'unbound-slot :instance nil :name (slot-definition-name slot))
            (error 'type-error :datum slot-value :expected-type type))))))

(def-transformer-wrapper unbound-writer
  (bind ((unbound-rdbms-value (make-array column-number :initial-element :null)))
    (lambda (slot-value rdbms-values index)
      (if (eq +unbound-slot-value+ slot-value)
          (replace rdbms-values unbound-rdbms-value :start1 index)
          (funcall function slot-value rdbms-values index)))))

(def-transformer-wrapper non-unbound-writer
  (lambda (slot-value rdbms-values index)
    (if (eq +unbound-slot-value+ slot-value)
        (if slot
            (error 'unbound-slot :instance nil :name (slot-definition-name slot))
            (error 'type-error :datum slot-value :expected-type type))
        (funcall function slot-value rdbms-values index))))

;;;;;;;;
;;; Null

(def-transformer-wrapper null-reader
  (lambda (rdbms-values index)
    (if (is-vector-of-constant rdbms-values :null index column-number)
        nil
        (funcall function rdbms-values index))))

(def-transformer-wrapper non-null-reader
  (lambda (rdbms-values index)
    (prog1-bind slot-value (funcall function rdbms-values index)
      (unless slot-value
        (if slot
            (error 'slot-type-error :slot slot :datum slot-value :expected-type type)
            (error 'type-error :datum slot-value :expected-type type))))))

(def-transformer-wrapper null-writer
  (bind ((nil-rdbms-values (make-array column-number :initial-element :null)))
    (lambda (slot-value rdbms-values index)
      (if slot-value
          (funcall function slot-value rdbms-values index)
          (replace rdbms-values nil-rdbms-values :start1 index)))))

(def-transformer-wrapper non-null-writer
  (lambda (slot-value rdbms-values index)
    (if slot-value
        (funcall function slot-value rdbms-values index)
        (if slot
            (error 'slot-type-error :slot slot :datum slot-value :expected-type type)
            (error 'type-error :datum slot-value :expected-type type)))))

;;;;;;;;;;;;;;;;;;;
;;; Unbound or null

(def-transformer-wrapper unbound-or-null-reader
  (bind ((rdbms-column-values (make-array column-number :initial-element :null))
         (unbound-rdbms-values (aprog1 (copy-seq rdbms-column-values)
                                 (setf (elt it 0) #f)))
         (nil-rdbms-values (aprog1 (copy-seq rdbms-column-values)
                             (setf (elt it 0) #t))))
    (lambda (rdbms-values index)
      (flet ((equal-vector (vector-1 vector-2 index length)
               (iter (for i-1 :from 0)
                     (for i-2 :from index)
                     (repeat length)
                     (unless (eq (elt vector-1 i-1) (elt vector-2 i-2))
                       (return-from equal-vector #f)))
               #t))
        (cond ((equal-vector unbound-rdbms-values rdbms-values index column-number)
               +unbound-slot-value+)
              ((equal-vector nil-rdbms-values rdbms-values index column-number)
               nil)
              (t (funcall function rdbms-values (1+ index))))))))

(def-transformer-wrapper unbound-or-null-writer
  (bind ((rdbms-column-values (make-array column-number :initial-element :null))
         (unbound-rdbms-values (aprog1 (copy-seq rdbms-column-values)
                                 (setf (elt it 0) #f)))
         (nil-rdbms-values (aprog1 (copy-seq rdbms-column-values)
                             (setf (elt it 0) #t))))
    (lambda (slot-value rdbms-values index)
      (cond ((eq +unbound-slot-value+ slot-value)
             (replace rdbms-values unbound-rdbms-values :start1 index))
            ((null slot-value)
             (replace rdbms-values nil-rdbms-values :start1 index))
            (t
             (setf (elt rdbms-values index) #t)
             (funcall function slot-value rdbms-values (1+ index)))))))

;;;;;;;;;;;;;;
;;; Serialized

(defun byte-vector->object-reader (rdbms-values index)
  (with-input-from-sequence (stream (elt rdbms-values index))
    (restore stream)))

(defun object->byte-vector-writer (slot-value rdbms-values index)
  (setf (elt rdbms-values index)
        (with-output-to-sequence (stream)
          (store slot-value stream))))

(defun base64->object-reader (rdbms-values index)
  (with-input-from-sequence (stream
    (with-input-from-string (base64 (elt rdbms-values index))
      (decode-base64-bytes base64)))
    (restore stream)))

(defun object->base64-writer (slot-value rdbms-values index)
  (setf (elt rdbms-values index)
        (with-output-to-string (base64)
          (encode-base64-bytes
            (with-output-to-sequence (stream)
              (store slot-value stream))
           base64))))

;;;;;;;;;;;;
;;; Identity

(defun identity-reader (rdbms-values index)
  (elt rdbms-values index))

(defun identity-writer (slot-value rdbms-values index)
  (setf (elt rdbms-values index) slot-value))

;;;;;;;;;;
;;; Number

(defun object->number-reader (rdbms-values index)
  (bind ((value (elt rdbms-values index)))
    (if (typep value 'number)
        value
        (parse-number value))))

;;;;;;;;;;;
;;; Integer

(defun object->integer-reader (rdbms-values index)
  (bind ((value (elt rdbms-values index)))
    (if (typep value 'number)
        value
        (parse-integer value))))

;;;;;;;;;;
;;; Symbol

(defun string->symbol-reader (rdbms-values index)
  (symbol-from-canonical-name (elt rdbms-values index)))

(defun symbol->string-writer (slot-value rdbms-values index)
  (setf (elt rdbms-values index) (canonical-symbol-name slot-value)))

;;;;;;;;
;;; List

(defun string->list-reader (rdbms-values index)
  (read-from-string (elt rdbms-values index)))

(defun list->string-writer (slot-value rdbms-values index)
  (setf (elt rdbms-values index) (write-to-string slot-value)))

;;;;;;;;;;;
;;; Boolean

(defun char->boolean-reader (rdbms-values index)
  (bind ((value (elt rdbms-values index)))
    (cond ((eq #\t value) #t)
          ((eq #\f value) #f)
          (t (error 'type-error :datum value :expected-type 'boolean)))))

(defun boolean->char-writer (slot-value rdbms-values index)
  (setf (elt rdbms-values index)
        (if slot-value
            #\t
            #\f)))

(defun integer->boolean-reader (rdbms-values index)
  (bind ((value (elt rdbms-values index)))
    (cond ((= 0 value) #t)
          ((= 1 value) #f)
          (t (error 'type-error :datum value :expected-type 'boolean)))))

(defun boolean->integer-writer (slot-value rdbms-values index)
  (setf (elt rdbms-values index)
        (if slot-value
            1
            0)))

(defun string->boolean-reader (rdbms-values index)
  (bind ((value (elt rdbms-values index)))
    (cond ((equal "t" value) #t)
          ((equal "f" value) #f)
          (t (error 'type-error :datum value :expected-type 'boolean)))))

(defun boolean->string-writer (slot-value rdbms-values index)
  (setf (elt rdbms-values index)
        (if slot-value
            "TRUE"
            "FALSE")))

(defun object->boolean-reader (rdbms-values index)
  (bind ((value (elt rdbms-values index)))
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
    (lambda (rdbms-values index)
      (bind ((value (elt rdbms-values index)))
        (aif (nth value member-elements)
             it
             (error 'type-error :datum value :expected-type type))))))

(defun member->integer-writer (type)
  (bind ((member-elements (slot-definition-type-member-elements type)))
    (lambda (slot-value rdbms-values index)
      (block found
        (loop for i from 0
              for value in member-elements
              when (eq value slot-value)
              do (progn
                   (setf (elt rdbms-values index) i)
                   (return-from found)))
        (error 'type-error :datum slot-value :expected-type type)))))

(defun string->member-reader (type)
  (bind ((member-elements (slot-definition-type-member-elements type)))
    (lambda (rdbms-values index)
      (aprog1 (string->symbol-reader rdbms-values index)
        (assert (member it member-elements))))))

(defun member->string-writer (type)
  (bind ((member-elements (slot-definition-type-member-elements type)))
    (lambda (slot-value rdbms-values index)
      (assert (member slot-value member-elements))
      (setf (elt rdbms-values index) (symbol->string-writer slot-value rdbms-values index)))))

;;;;;;;;;;;;;;;;;
;;; Date and time

(defun string->local-time-reader (rdbms-values index)
  (bind ((*default-timezone* +utc-zone+))
    (parse-timestring (elt rdbms-values index) :date-time-separator #\Space)))

(defun integer->local-time-reader (rdbms-values index)
  ;; NOTE: assuming that the database server is configured to UTC time zone
  (local-time :universal (elt rdbms-values index) :timezone +utc-zone+))

(defun date->string-writer (slot-value rdbms-values index)
  (assert (eq (timezone-of slot-value) +utc-zone+))
  (setf (elt rdbms-values index) (format-timestring slot-value :omit-time-part-p #t)))

(defun time->string-writer (slot-value rdbms-values index)
  (assert (eq (timezone-of slot-value) +utc-zone+))
  (setf (elt rdbms-values index) (format-timestring slot-value :omit-date-part-p #t :omit-timezone-part-p #t)))

(defun timestamp->string-writer (slot-value rdbms-values index)
  (assert (eq (timezone-of slot-value) +utc-zone+))
  (setf (elt rdbms-values index) (format-timestring slot-value :date-time-separator #\Space :use-zulu-p #f)))

(defun local-time->integer-writer (slot-value rdbms-values index)
  (assert (eq (timezone-of slot-value) +utc-zone+))
  (setf (elt rdbms-values index) (universal-time slot-value)))

;;;;;;;;;;
;;; Object

(defun object-reader (rdbms-values index)
  (load-instance (rdbms-values->oid* rdbms-values index) :skip-existence-check #t))

(defun object-writer (slot-value rdbms-values index)
  (oid->rdbms-values* (oid-of slot-value) rdbms-values index))
