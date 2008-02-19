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

(def (function io) slot-boundp-or-value-using-class-t (class instance slot return-with)
  (assert-instance-slot-correspondence)
  (flet ((return-value (value)
           (return-from slot-boundp-or-value-using-class-t
             (funcall return-with
                      (if (single-values-having-validity-p value)
                          (elt-0 (values-of value))
                          value)))))
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

(def (function io) (setf slot-boundp-or-value-using-class-t) (new-value class instance slot)
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


(defmethod slot-value-using-class ((class persistent-class-t)
                                   (instance persistent-object)
                                   (slot persistent-effective-slot-definition-t))
  "Reads the slot value from the database or the cache."
  (slot-boundp-or-value-using-class-t class instance slot
                                      (lambda (value)
                                        (flet ((check-value (value)
                                                 (when (unbound-slot-marker-p value)
                                                   (slot-unbound-t instance slot))))
                                          (if (values-having-validity-p value)
                                              (iter (for (v s e) :in-values-having-validity value)
                                                    (check-value v))
                                              (check-value value))
                                          value))))

(defmethod (setf slot-value-using-class) (new-value
                                          (class persistent-class-t)
                                          (instance persistent-object)
                                          (slot persistent-effective-slot-definition-t))
  "Writes the new slot value to the database and the cache."
  (setf (slot-boundp-or-value-using-class-t class instance slot) new-value))

(defmethod slot-boundp-using-class ((class persistent-class-t)
                                    (instance persistent-object)
                                    (slot persistent-effective-slot-definition-t))
  "Reads boundness from the database or the cache."
  (slot-boundp-or-value-using-class class instance slot
                                    (lambda (value)
                                      (if (values-having-validity-p value)
                                          (iter (for (v s e) :in-values-having-validity value)
                                                (always (not (unbound-slot-marker-p v))))
                                          (not (unbound-slot-marker-p value))))))

(defmethod slot-makunbound-using-class ((class persistent-class-t)
                                        (instance persistent-object)
                                        (slot persistent-effective-slot-definition-t))
  "Writes boundness to the database and the cache."
  (setf (slot-boundp-or-value-using-class-t class instance slot) +unbound-slot-marker+)
  instance)
