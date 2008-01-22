;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;;;
;;; Model

(defclass* values-having-validity ()
  ((values :type (vector t))
   (validity-starts :type (vector timestamp))
   (validity-ends :type (vector timestamp))))

(defun make-single-values-having-validity (value validity-start validity-end)
  (flet ((make-single-element-vector (element)
           (aprog1 (make-array 1)
             (setf (aref it 0) element))))
    (make-instance 'values-having-validity
                   :values (make-single-element-vector value)
                   :validity-starts (make-single-element-vector validity-start)
                   :validity-ends (make-single-element-vector validity-end))))

(defun single-values-having-validity-p (values-having-validity)
  (= 1 (length (values-of values-having-validity))))

(defprint-object (instance values-having-validity)
  (write-char #\{)
  (iter (for value :in-sequence (values-of instance))
        (unless (first-iteration-p)
          (write-string ", "))
        (write value))
  (write-char #\}))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Iteratation support

(defmacro-clause (for variables :in-values-having-validity v)
  "Use it like (for (value validity-start validity-end index) :in-values-having-validity v)"
  (with-unique-names (value values validity-starts validity-ends)
    (let ((index (or (fourth variables) (gensym "INDEX-"))))
      `(progn
        (with ,value = ,v)
        (with ,values = (values-of ,value))
        (with ,validity-starts = (validity-starts-of ,value))
        (with ,validity-ends = (validity-ends-of ,value))
        (for ,index :from 0 :below (length ,values))
        (for ,(first variables) = (aref ,values ,index))
        (for ,(second variables) = (aref ,validity-starts ,index))
        (for ,(third variables) = (aref ,validity-ends ,index))))))

;;;;;;;;;;;;;;
;;; Functional

(defun collect-values-having-validity (value-holders value-function validity-start-function validity-end-function no-value-function requested-validity-start requested-validity-end)
  "From a list of ordered (by t) tuples each containing a value, a validity start and a validity end returns the corresponding values-having-validity for the requested range. May return a simple value instead of a values-having-validity."
  (if (zerop (length value-holders))
      (funcall no-value-function requested-validity-start requested-validity-end)
      (bind ((values (make-array 4 :adjustable #t :fill-pointer 0))
             (validity-starts (make-array 4 :adjustable #t :fill-pointer 0))
             (validity-ends (make-array 4 :adjustable #t :fill-pointer 0))
             (indices (make-array 4 :adjustable #t :fill-pointer 0)))
        (labels ((push-value-having-validity (value validity-start validity-end)
                   ;;(format t "~%Push Start: ~A End: ~A Value: ~A" value validity-start validity-end)
                   (vector-push-extend value values)
                   (vector-push-extend validity-start validity-starts)
                   (vector-push-extend validity-end validity-ends)
                   (vector-push-extend (length indices) indices))
                 (%collect-values-having-validity (index validity-start validity-end)
                   ;;(format t "~%Collect Index: ~A Start: ~A End: ~A" index  validity-start validity-end)
                   (when (local-time< validity-start validity-end)
                     (when (< index (length value-holders))
                       (iter (for i :from index :below (length value-holders))
                             (for record = (elt value-holders i))
                             (for record-validity-start = (funcall validity-start-function record))
                             (for record-validity-end = (funcall validity-end-function record))
                             (for merged-validity-start = (local-time-max validity-start record-validity-start))
                             (for merged-validity-end = (local-time-min validity-end record-validity-end))
                             (when (local-time< merged-validity-start merged-validity-end)
                               (push-value-having-validity (funcall value-function record) merged-validity-start merged-validity-end)
                               (%collect-values-having-validity (1+ i) validity-start merged-validity-start)
                               (%collect-values-having-validity (1+ i) merged-validity-end validity-end)
                               (return-from %collect-values-having-validity))))
                     (push-value-having-validity
                      (funcall no-value-function validity-start validity-end)
                      validity-start validity-end))))
          (%collect-values-having-validity 0 requested-validity-start requested-validity-end)
          (if (= 1 (length values))
              (aref values 0)
              (progn
                (sort indices #'local-time< :key (lambda (index) (aref validity-starts index)))
                (permute values indices)
                (permute validity-starts indices)
                (permute validity-ends indices)
                (make-instance 'values-having-validity
                               :values values
                               :validity-starts validity-starts
                               :validity-ends validity-ends)))))))

(defun extract-values-having-validity (values-having-validity requested-validity-start requested-validity-end)
  "Extracts the requested range if possible, secondary value indicates success. May return a simple value instead of a values-having-validity."
  (bind ((validity-starts (validity-starts-of values-having-validity))
         (validity-ends (validity-ends-of values-having-validity))
         (first-validity-start (aref validity-starts 0))
         (last-validity-end (aref validity-ends (1- (length validity-ends)))))
    (if (and (local-time<= first-validity-start requested-validity-start)
             (local-time<= requested-validity-end last-validity-end))
        (bind ((values (make-array 8 :adjustable #t :fill-pointer 0))
               (validity-starts (make-array 8 :adjustable #t :fill-pointer 0))
               (validity-ends (make-array 8 :adjustable #t :fill-pointer 0))
               (result
                (make-instance 'values-having-validity
                               :values values
                               :validity-starts validity-starts
                               :validity-ends validity-ends)))
          (flet ((push-value-with-validity (value validity-start validity-end)
                   (vector-push-extend value values)
                   (vector-push-extend validity-start validity-starts)
                   (vector-push-extend validity-end validity-ends)))
            (iter (with in-requested-validity-range = #f)
                  (for (value validity-start validity-end) :in-values-having-validity values-having-validity)
                  (if in-requested-validity-range
                      (when (local-time<= validity-start requested-validity-end validity-end)
                        (push-value-with-validity value validity-start requested-validity-end)
                        (setf in-requested-validity-range #f))
                      (when (local-time<= validity-start requested-validity-start validity-end)
                        (push-value-with-validity value requested-validity-start validity-end)
                        (setf in-requested-validity-range #t)))))
          (values (if (= 1 (length values))
                      (aref values 0)
                      result)
                  #t))
        (values nil #f))))

;;;;;;;;;;;
;;; Utility

(def constant +t-delete+ 0
  "Constant used to mark RDBMS records for association slots.")

(def constant +t-insert+ 1
  "Constant used to mark RDBMS records for association slots.")

(defun collect-children-having-validity (child-slot records validity-start validity-end)
  ;; records are tuples ordered by t ascending: (child-oid validity-start validity-end action)
  ;; TODO: multiple column oid
  (labels ((child-of (record)
             (restore-slot-value child-slot record 0))
           (validity-start-of (record)
             (aref record 1))
           (validity-end-of (record)
             (aref record 2))
           (action-of (record)
             (aref record 3))
           (collect-children-having-validity (records validity-start validity-end)
             (bind (set)
               (iter (for record :in-sequence records)
                     (when (and (local-time<= validity-end (validity-end-of record))
                                (local-time<= (validity-start-of record) validity-start))
                       (ecase (action-of record)
                         (#.+t-insert+
                          (pushnew (child-of record) set))
                         (#.+t-delete+
                          (deletef (child-of record) set)))))
               set)))
    (bind (validities)
      (flet ((push-validity (validity)
               (pushnew validity validities :test #'local-time=)))
        (push-validity validity-start)
        (push-validity validity-end)
        (iter (for record :in-sequence records)
              (push-validity (validity-start-of record))
              (push-validity (validity-end-of record))))
      (setf validities (sort validities #'local-time<))
      (if (= 2 (length validities))
          (collect-children-having-validity records (first validities) (second validities))
          (bind ((size (1- (length validities)))
                 (values (make-array size :fill-pointer 0))
                 (validity-starts (make-array size :fill-pointer 0))
                 (validity-ends (make-array size :fill-pointer 0)))
            (iter (with value = nil)
                  (with previous-value = :unbound)
                  (with previous-validity = nil)
                  (for validity :in validities)
                  (for modified-validity = validity)
                  (for index :from -1)
                  (if previous-validity
                      (progn
                        (setf value (collect-children-having-validity records previous-validity modified-validity))
                        (if (equal value previous-value)
                            (decf index)
                            (progn
                              (vector-push value values)
                              (vector-push previous-validity validity-starts)
                              (vector-push modified-validity validity-ends)
                              (setf previous-value value)
                              (setf previous-validity validity))))
                      (setf previous-validity validity)))
            (if (= 1 (length values))
                (aref values 0)
                (make-instance 'values-having-validity
                               :values values
                               :validity-starts validity-starts
                               :validity-ends validity-ends)))))))

;; TODO: this failes when multiple records are present with the same t but overlapping validity ranges
;; (ordering for t does not affect the order of records) THIS MUST BE FORBIDDEN
(defun collect-single-slot-values-having-validity-from-records (instance slot records t-slot value-index)
  (collect-single-slot-values-having-validity
   instance slot records
   (lambda (record)
     (restore-slot-value t-slot record value-index))
   (lambda (record)
     (elt record 0))
   (lambda (record)
     (elt record 1))))

(defun collect-single-slot-values-having-validity-from-instances (instance slot t-instances t-slot)
  (collect-single-slot-values-having-validity
   instance slot t-instances
   (lambda (t-instance)
     (underlying-slot-boundp-or-value-using-class (class-of t-instance) t-instance t-slot))
   (lambda (t-instance)
     (validity-start-of t-instance))
   (lambda (t-instance)
     (validity-end-of t-instance))))

(defun collect-multiple-slot-values-having-validity-from-records (instance slots records t-slots)
  (iter (for slot :in slots)
        (for t-slot :in t-slots)
        (for value-index :initially 2 :then (+ value-index (length (columns-of t-slot))))
        (collect (collect-single-slot-values-having-validity-from-records instance slot records t-slot value-index))))

(defun collect-multiple-slot-values-having-validity-from-instances (instance slots t-instances t-slots)
  (iter (for slot :in slots)
        (for t-slot :in t-slots)
        (collect (collect-single-slot-values-having-validity-from-instances instance slot t-instances t-slot))))

(defun collect-single-slot-values-having-validity (instance slot value-holders value-function validity-start-function validity-end-function)
  (collect-values-having-validity
   value-holders value-function validity-start-function validity-end-function
   (lambda ()
     (slot-unbound-t :instance instance :slot slot))
   *validity-start* *validity-end*))
