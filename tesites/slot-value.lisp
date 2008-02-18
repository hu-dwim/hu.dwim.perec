;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;;;;;;;;;;;;;
;;; Error signaling

(defcondition* unbound-slot-t (unbound-slot)
  ((t-value :type timestamp)
   (validity-start :type timestamp)
   (validity-end :type timestamp))
  (:report (lambda (condition stream)
             (format stream "The slot ~S is unbound in the object ~S"
                     (cell-error-name condition)
                     (unbound-slot-instance condition))
             (when (slot-boundp condition 't-value)
               (format stream " with t ~A"
                       (t-value-of condition)))
             (when (slot-boundp condition 'validity-start)
               (when (slot-boundp condition 't-value)
                 (format stream " and"))
               (format stream " with validity range ~A -> ~A"
                       (validity-start-of condition)
                       (validity-end-of condition))))))

(defun slot-unbound-t (instance slot
                       &key (t-value nil t-value-p) (validity-start nil validity-start-p) (validity-end nil validity-end-p))
  (apply 'error 'unbound-slot-t
         :name (slot-definition-name slot)
         :instance instance
         (append
          (when (or t-value-p
                    (boundp '*t*))
            (list :t-value (or t-value
                         *t*)))
          (when (or validity-start-p
                    (boundp '*validity-start*))
            (list :validity-start (or validity-start
                                      *validity-start*)))
          (when (or validity-end-p
                    (boundp '*validity-end*))
            (list :validity-end (or validity-end
                                    *validity-end*))))))

;;;;;;;;;;;;;;
;;; Integrated

(defun integrated-time-dependent-slot-value (instance slot-name)
  (bind ((slot-values (slot-value instance slot-name)))
    (if (typep slot-values 'values-having-validity)
        (iter (for (value validity-start validity-end index) :in-values-having-validity slot-values)
              (summing (* value (local-time- validity-end validity-start))))
        (* slot-values (local-time- *validity-end* *validity-start*)))))

(defun (setf integrated-time-dependent-slot-value) (new-value instance slot-name)
  (setf (slot-value instance slot-name)
        (/ new-value (local-time- *validity-end* *validity-start*))))


;;;;;;;;;;;;
;;; Averaged

(defun averaged-time-dependent-slot-value (instance slot-name)
  (bind ((slot-values (slot-value instance slot-name)))
    (if (typep slot-values 'values-having-validity)
        (iter (for (value validity-start validity-end index) :in-values-having-validity slot-values)
              (summing (* value (local-time- validity-end validity-start)) :into sum)
              (finally
               (return (/ sum (local-time- *validity-end* *validity-start*)))))
        slot-values)))

(defun (setf averaged-time-dependent-slot-value) (new-value instance slot-name)
  (setf (slot-value instance slot-name) new-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Slot value and friends

(defmethod slot-value-using-class ((class persistent-class-t)
                                   (instance persistent-object)
                                   (slot persistent-effective-slot-definition-t))
  (assert-instance-slot-correspondence)
  (flet ((return-value (value)
           (return-from slot-value-using-class
             (if (single-values-having-validity-p value)
                 (elt-0 (values-of value))
                 value))))
    (bind ((persistent (persistent-p instance)))
      (assert-instance-access instance persistent)
      (bind (((values slot-value-cached cached-value) (slot-value-cached-p instance slot)))
        (when (or (not persistent)
                  (and *cache-slot-values*
                       slot-value-cached))
          (if (time-dependent-p slot)
              (progn
                *validity-start* *validity-end*
                (when (temporal-p slot)
                  *t*)
                (if (unbound-slot-marker-p cached-value)
                    (slot-unbound-t instance slot)
                    (bind (((values value covers-validity-range-p)
                            (extract-values-having-validity cached-value *validity-start* *validity-end*)))
                      (if covers-validity-range-p
                          (return-value value)
                          (unless persistent
                            (slot-unbound-t instance slot))))))
              (progn
                *t*
                (if (unbound-slot-marker-p cached-value)
                    (slot-unbound-t instance slot)
                    (return-value cached-value)))))
        (bind ((value (restore-slot class instance slot)))
          (setf (underlying-slot-value-using-class class instance slot)
                (if (values-having-validity-p value)
                    value
                    (make-single-values-having-validity value *validity-start* *validity-end*)))
          (return-value value))))))

(defmethod (setf slot-value-using-class) (new-value
                                          (class persistent-class-t)
                                          (instance persistent-object)
                                          (slot persistent-effective-slot-definition-t))
  (assert-instance-slot-correspondence)
  (bind ((persistent (persistent-p instance))
         (new-value (if (and (time-dependent-p slot)
                             (not (values-having-validity-p new-value)))
                        (make-single-values-having-validity new-value *validity-start* *validity-end*)
                        new-value)))
    (assert-instance-access instance persistent)
    (if persistent
        (store-slot class instance slot new-value)
        (assert (not (underlying-slot-boundp-using-class class instance slot))))
    (when (or (not persistent)
              (and *cache-slot-values*
                   (cache-p slot)))
      (setf (underlying-slot-value-using-class class instance slot) new-value)))
  new-value)

(defmethod slot-boundp-using-class ((class persistent-class-t)
                                    (instance persistent-object)
                                    (slot persistent-effective-slot-definition-t))
  ;; TODO: cache slot values and refactor
  (when (persistent-p instance)
    (handler-case
        (slot-value-using-class class instance slot)
      (unbound-slot-t (e)
                      (declare (ignore e))
                      (return-from slot-boundp-using-class #f)))
    #t)
  #+nil
  (error "Not yet implemented"))

(defmethod slot-makunbound-using-class ((class persistent-class-t)
                                        (instance persistent-object)
                                        (slot persistent-effective-slot-definition-t))
  (error "Not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;
;;; Association slots 

;; TODO: slot value using class should not be different for association slots
;; TODO: it should simply handle cache and database store according to the rules
(defmethod slot-value-using-class ((class persistent-class-t)
                                   (instance persistent-object)
                                   (slot persistent-association-end-effective-slot-definition-t))
  (assert-instance-slot-correspondence)
  (bind ((persistent (persistent-p instance)))
    (assert-instance-access instance persistent)
    (if (eq :1 (cardinality-kind-of (child-slot-of slot)))
        (select-1-1-association-t-record class instance slot)
        (select-1-n-association-t-records class instance slot))))

(defmethod (setf slot-value-using-class) (new-value
                                          (class persistent-class-t)
                                          (instance persistent-object)
                                          (slot persistent-association-end-effective-slot-definition-t))
  (assert-instance-slot-correspondence)
  (bind ((persistent (persistent-p instance)))
    (assert-instance-access instance persistent)
    (if (eq :1 (cardinality-kind-of (child-slot-of slot)))
        (insert-1-1-association-t-record instance slot new-value)
        ;; TODO: get first and delete those
        (insert-1-n-association-delta-t-records instance slot new-value +t-insert+))))
