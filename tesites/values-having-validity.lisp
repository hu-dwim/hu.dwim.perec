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

(def (function e) values-having-validity-p (instance)
  (typep instance 'values-having-validity))

(def (function e) elt-values-having-validity (instance index)
  (values (elt (values-of instance) index)
          (elt (validity-starts-of instance) index)
          (elt (validity-ends-of instance) index)))

(def (function e) values-having-validity= (values-1 values-2)
  (and (length (values-of values-1))
       (length (values-of values-2))
       (iter (for index :from 0 :below (length (values-of values-1)))
             (bind (((:values v-1 s-1 e-1)
                     (elt-values-having-validity values-1 index))
                    ((:values v-2 s-2 e-2)
                     (elt-values-having-validity values-2 index)))
               (always (equal v-1 v-2))
               (always (local-time= s-1 s-2))
               (always (local-time= e-1 e-2))))))

(def (function e) make-single-values-having-validity (value validity-start validity-end)
  (flet ((make-single-element-vector (element)
           (aprog1 (make-array 1)
             (setf (aref it 0) element))))
    (make-instance 'values-having-validity
                   :values (make-single-element-vector value)
                   :validity-starts (make-single-element-vector validity-start)
                   :validity-ends (make-single-element-vector validity-end))))

(def (function e) single-values-having-validity-p (instance)
  (and (values-having-validity-p instance)
       (= 1 (length (values-of instance)))))

(def (function e) make-values-having-validity (values validity-starts validity-ends)
  (make-instance 'values-having-validity
                 :values (coerce values 'simple-vector)
                 :validity-starts (coerce validity-starts 'simple-vector)
                 :validity-ends (coerce validity-ends 'simple-vector)))

(def print-object values-having-validity
  (write-char #\{)
  (iter (for value :in-sequence (values-of self))
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

;;;;;;;;;;;;;;;;;;;;;;;
;;; For primitive types

(def function collect-values-having-validity (value-holders value-function validity-start-function validity-end-function no-value-function requested-validity-start requested-validity-end)
  "From a list of ordered (by t) tuples each containing a value, a validity start and a validity end returns the corresponding values-having-validity for the requested range."
  (assert (not (local-time= requested-validity-start requested-validity-end)))
  (if (zerop (length value-holders))
      (make-single-values-having-validity (funcall no-value-function requested-validity-start requested-validity-end) requested-validity-start requested-validity-end)
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
          (sort indices #'local-time< :key (lambda (index) (aref validity-starts index)))
          (permute values indices)
          (permute validity-starts indices)
          (permute validity-ends indices)
          (make-instance 'values-having-validity
                         :values values
                         :validity-starts validity-starts
                         :validity-ends validity-ends)))))

(def function extract-values-having-validity (values-having-validity requested-validity-start requested-validity-end)
  "Extracts the requested range if possible, secondary value indicates success."
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
          (values result #t))
        (values nil #f))))

;;;;;;;;;;;;;;;;;
;;; For set types

(def constant +t-delete+ 0
  "Constant used to mark RDBMS records for association slots.")

(def constant +t-insert+ 1
  "Constant used to mark RDBMS records for association slots.")

(def function collect-children-having-validity (value-holders value-function validity-start-function validity-end-function action-function requested-validity-start requested-validity-end)
  "Collect children sets as values-having-validity, value-holders must be ordered by t ascending."
  (labels ((%collect-children-having-validity (value-holders validity-start validity-end)
             (bind ((set nil))
               (iter (for value :in-sequence value-holders)
                     (when (and (local-time<= validity-end (funcall validity-end-function value))
                                (local-time<= (funcall validity-start-function value) validity-start))
                       (ecase (funcall action-function value)
                         (#.+t-insert+
                          (pushnew (funcall value-function value) set))
                         (#.+t-delete+
                          (deletef (funcall value-function value) set)))))
               set)))
    (bind ((validities nil))
      (flet ((push-validity (validity)
               (pushnew validity validities :test #'local-time=)))
        (push-validity requested-validity-start)
        (push-validity requested-validity-end)
        (iter (for value :in-sequence value-holders)
              (push-validity (funcall validity-start-function value))
              (push-validity (funcall validity-end-function value))))
      (setf validities (sort validities #'local-time<))
      (if (= 2 (length validities))
          (make-single-values-having-validity (%collect-children-having-validity value-holders (first validities) (second validities)) (first validities) (second validities))
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
                        (setf value (%collect-children-having-validity value-holders previous-validity modified-validity))
                        (if (equal value previous-value)
                            (decf index)
                            (progn
                              (vector-push value values)
                              (vector-push previous-validity validity-starts)
                              (vector-push modified-validity validity-ends)
                              (setf previous-value value)
                              (setf previous-validity validity))))
                      (setf previous-validity validity)))
            (make-instance 'values-having-validity
                           :values values
                           :validity-starts validity-starts
                           :validity-ends validity-ends))))))

;;;;;;;;;;;
;;; Utility

;; TODO: review this
;; TODO: this failes when multiple records are present with the same t but overlapping validity ranges
;; (ordering for t does not affect the order of records) THIS MUST BE FORBIDDEN
(def function collect-single-slot-values-having-validity-from-records (instance slot records h-slot value-index)
  (collect-single-slot-values-having-validity
   instance slot records
   (lambda (record)
     (restore-slot-value h-slot record value-index))
   (lambda (record)
     (elt record 0))
   (lambda (record)
     (elt record 1))))

(def function collect-single-slot-values-having-validity-from-instances (instance slot h-instances h-slot)
  (collect-single-slot-values-having-validity
   instance slot h-instances
   (lambda (h-instance)
     (underlying-slot-boundp-or-value-using-class (class-of h-instance) h-instance h-slot))
   (lambda (h-instance)
     (validity-start-of h-instance))
   (lambda (h-instance)
     (validity-end-of h-instance))))

(def function collect-multiple-slot-values-having-validity-from-records (instance slots records h-slots)
  (iter (for slot :in slots)
        (for h-slot :in h-slots)
        (for value-index :initially 2 :then (+ value-index (length (columns-of h-slot))))
        (collect (collect-single-slot-values-having-validity-from-records instance slot records h-slot value-index))))

(def function collect-multiple-slot-values-having-validity-from-instances (instance slots h-instances h-slots)
  (iter (for slot :in slots)
        (for h-slot :in h-slots)
        (collect (collect-single-slot-values-having-validity-from-instances instance slot h-instances h-slot))))

(def function collect-single-slot-values-having-validity (instance slot value-holders value-function validity-start-function validity-end-function)
  (collect-values-having-validity
   value-holders value-function validity-start-function validity-end-function
   (lambda (validity-start validity-end)
     (slot-unbound-t instance slot :validity-start validity-start :validity-end validity-end))
   *validity-start* *validity-end*))
