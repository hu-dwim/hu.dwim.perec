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

(def function slot-unbound-t (instance slot
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

(def function integrated-time-dependent-slot-value (instance slot-name &optional (ignore-nil #f))
  (bind ((slot-values (slot-value instance slot-name)))
    (if (typep slot-values 'values-having-validity)
        (iter (for (validity-start validity-end value) :in-values-having-validity slot-values)
              (unless (and ignore-nil
                           (not value))
                (summing (* value (timestamp-difference validity-end validity-start)))))
        (if (and ignore-nil
                 (not slot-values))
            0
            (* slot-values (timestamp-difference *validity-end* *validity-start*))))))

(def function (setf integrated-time-dependent-slot-value) (new-value instance slot-name)
  (setf (slot-value instance slot-name)
        (/ new-value (timestamp-difference *validity-end* *validity-start*))))

(def (definer e :available-flags "e") slot-integrator (integrated-slot-name slot-name)
  (bind ((integrated-slot-name (concatenate-symbol integrated-slot-name "-of")))
    `(progn
       (def (function e) ,integrated-slot-name (instance)
         (integrated-time-dependent-slot-value instance ',slot-name))

       (def (function e) (setf ,integrated-slot-name) (new-value instance)
         (setf (integrated-time-dependent-slot-value instance ',slot-name) new-value)))))

;;;;;;;;;;;;
;;; Averaged

(def function averaged-time-dependent-slot-value (instance slot-name &optional (ignore-nil #f))
  (/ (integrated-time-dependent-slot-value instance slot-name ignore-nil)
     (timestamp-difference *validity-end* *validity-start*)))

(def function (setf averaged-time-dependent-slot-value) (new-value instance slot-name)
  (setf (slot-value instance slot-name) new-value))

(def (definer e :available-flags "e") slot-averager (averaged-slot-name slot-name)
  (bind ((averaged-slot-name (concatenate-symbol averaged-slot-name "-of")))
    `(progn
       (def (function e) ,averaged-slot-name (instance)
         (averaged-time-dependent-slot-value instance ',slot-name))

       (def (function e) (setf ,averaged-slot-name) (new-value instance)
         (setf (averaged-time-dependent-slot-value instance ',slot-name) new-value)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Slot value and friends

;; I modified this method to return +unbound-slot-marker+ instead of signalling unbound-slot-t.
;; The special value was checked in slot-value-using-class and slot-boundp-using-class anyway. (tomi)
(def (function io) slot-boundp-or-value-using-class-t (class instance slot)
  (assert-instance-slot-correspondence)
  (assert (implies (time-dependent-p slot) (boundp '*validity-start*))
          () 'unbound-variable :name '*validity-start*)
  (assert (implies (time-dependent-p slot) (boundp '*validity-end*))
          () 'unbound-variable :name '*validity-end*)
  (assert (implies (temporal-p slot) (boundp '*t*))  () 'unbound-variable :name '*t*)
  
  (flet ((return-value (value)
           (return-from slot-boundp-or-value-using-class-t value)))
    (bind ((persistent (persistent-p instance))
           (cache-p (and *cache-slot-values* (cache-p slot))))
      (assert-instance-access instance persistent)
      (bind (((:values slot-value-cached cached-value) (slot-value-cached-p instance slot)))
        (when (or (not persistent)
                  (and cache-p slot-value-cached))
          (if (time-dependent-p slot)
              (if (unbound-slot-marker-p cached-value)
                  (return-value +unbound-slot-marker+)
                  (bind (((:values value covers-validity-range-p)
                          (values-having-validity-value cached-value *validity-start* *validity-end*)))
                    (if covers-validity-range-p
                        (return-value value)
                        (unless persistent
                          (return-value +unbound-slot-marker+)))))
              (return-value cached-value)))
        (aprog1 (restore-slot class instance slot)
          (when (or (not persistent) cache-p)
            (setf (underlying-slot-value-using-class class instance slot) it)))))))

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


(defmethod slot-value-using-class ((class persistent-class)
                                   (instance persistent-object)
                                   (slot persistent-effective-slot-definition-t))
  "Reads the slot value from the database or the cache."
  (bind ((value (slot-boundp-or-value-using-class-t class instance slot)))
    (flet ((check-value (value)
             (when (unbound-slot-marker-p value)
               (slot-unbound-t instance slot))))
      (if (values-having-validity-p value)
          (iter (for (s e v) :in-values-having-validity value)
                (check-value v))
          (check-value value))
      value)))

(defmethod (setf slot-value-using-class) (new-value
                                          (class persistent-class)
                                          (instance persistent-object)
                                          (slot persistent-effective-slot-definition-t))
  "Writes the new slot value to the database and the cache."
  (setf (slot-boundp-or-value-using-class-t class instance slot) new-value))

(defmethod slot-boundp-using-class ((class persistent-class)
                                    (instance persistent-object)
                                    (slot persistent-effective-slot-definition-t))
  "Reads boundness from the database or the cache."
  (bind ((value (slot-boundp-or-value-using-class-t class instance slot)))
    (if (values-having-validity-p value)
        (iter (for (s e v) :in-values-having-validity value)
              (always (not (unbound-slot-marker-p v))))
        (not (unbound-slot-marker-p value)))))

(defmethod slot-makunbound-using-class ((class persistent-class)
                                        (instance persistent-object)
                                        (slot persistent-effective-slot-definition-t))
  "Writes boundness to the database and the cache."
  (setf (slot-boundp-or-value-using-class-t class instance slot) +unbound-slot-marker+)
  instance)

