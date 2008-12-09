;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(def (special-variable e) *simplify-d-values* #f)

(def (function io) simplify-d-value (d-value)
  (if (and *simplify-d-values* (single-d-value-p d-value))
      (single-d-value d-value)
      d-value))

(def (condition* e) unbound-slot-d (unbound-slot)
  ((coordinates :type list))
  (:report (lambda (condition stream)
             (format stream "The slot ~S is unbound in the object ~S with coordinates ~S."
                     (cell-error-name condition)
                     (unbound-slot-instance condition)
                     (coordinates-of condition)))))

(def function slot-unbound-d (instance slot coordinates)
  (error 'unbound-slot-d
         :name (slot-definition-name slot)
         :instance instance
         :coordinates coordinates))

(def (function io) collect-coordinates-from-variables (dimensions)
  ;; TODO asserts for read/write
  (iter (for dimension :in dimensions)
        (typecase dimension
          (ordering-dimension
           (collect (coordinate dimension)))
          (t
           (bind ((coordinate (coordinate dimension)))
             (collect (if (whole-domain-marker-p coordinate)
                          coordinate
                          (ensure-list coordinate))))))))

(def (function io) slot-boundp-or-value-using-class-d (class instance slot coordinates)
  (assert-instance-slot-correspondence)
  (bind ((persistent (persistent-p instance))
         (cache-p (and *cache-slot-values* (cache-p slot))))
    (assert-instance-access instance persistent)
    (bind (((:values slot-value-cached cached-value) (slot-value-cached-p instance slot)))
      (when (or (not persistent)
                (and cache-p slot-value-cached))
        (if (unbound-slot-marker-p cached-value)
            (return-from slot-boundp-or-value-using-class-d +unbound-slot-marker+)
            (bind ((d-value (value-at-coordinates cached-value coordinates)))
              (when (covering-d-value-p d-value coordinates)
                (return-from slot-boundp-or-value-using-class-d (simplify-d-value d-value))))))
      (if persistent
          (bind ((d-value (restore-slot class instance slot :coordinates coordinates))) ;; FIXME returns ii
            (when (or (not persistent) cache-p)
              (if (d-value-p cached-value)
                  (setf (into-d-value cached-value) d-value)
                  (setf (underlying-slot-value-using-class class instance slot) d-value)))
            (simplify-d-value d-value))
          (return-from slot-boundp-or-value-using-class-d +unbound-slot-marker+)))))

(def (function io) (setf slot-boundp-or-value-using-class-d) (new-value class instance slot)
  (assert-instance-slot-correspondence)
  (bind ((persistent (persistent-p instance))
         (cache-p (and *cache-slot-values* (cache-p slot)))
         (new-value (if (not (d-value-p new-value))
                        (make-single-d-value (dimensions-of slot)
                                             (collect-coordinates-from-variables (dimensions-of slot))
                                             new-value)
                        new-value)))
    (assert-instance-access instance persistent)
    (when persistent
      (store-slot class instance slot new-value))
    (when (or (not persistent) cache-p)
      (if (inheriting-dimension-index-of slot)
          ;; TODO update the cache (needs selects) instead of overwriting it
          (setf (underlying-slot-value-using-class class instance slot) new-value)
          (bind (((:values slot-value-cached cached-value) (slot-value-cached-p instance slot)))
            (if (and slot-value-cached (d-value-p cached-value))
                (setf (into-d-value cached-value) new-value) 
                (setf (underlying-slot-value-using-class class instance slot) new-value))))))
  new-value)

(defmethod slot-value-using-class ((class persistent-class)
                                   (instance persistent-object)
                                   (slot persistent-effective-slot-definition-d))
  "Reads the slot value from the database or the cache."
  (bind ((coordinates (collect-coordinates-from-variables (dimensions-of slot)))
         (value (slot-boundp-or-value-using-class-d class instance slot coordinates)))
    (flet ((check-value (value coordinates)
             (when (unbound-slot-marker-p value)
               (slot-unbound-d instance slot coordinates))))
      (if (d-value-p value)
          (iter (for c-value :in (c-values-of value))
                (check-value (value-of c-value) (coordinates-of c-value)))
          (check-value value coordinates))
      value)))

(defmethod (setf slot-value-using-class) (new-value
                                          (class persistent-class)
                                          (instance persistent-object)
                                          (slot persistent-effective-slot-definition-d))
  "Writes the new slot value to the database and the cache."
  (setf (slot-boundp-or-value-using-class-d class instance slot) new-value))

(defmethod slot-boundp-using-class ((class persistent-class)
                                    (instance persistent-object)
                                    (slot persistent-effective-slot-definition-d))
  "Reads boundness from the database or the cache."
  (bind ((coordinates (collect-coordinates-from-variables (dimensions-of slot)))
         (value (slot-boundp-or-value-using-class-d class instance slot coordinates)))
    (if (d-value-p value)
        (iter (for c-value :in (c-values-of value))
              (always (not (unbound-slot-marker-p (value-of c-value)))))
        (not (unbound-slot-marker-p value)))))

(defmethod slot-makunbound-using-class ((class persistent-class)
                                        (instance persistent-object)
                                        (slot persistent-effective-slot-definition-d))
  "Writes boundness to the database and the cache."
  (setf (slot-boundp-or-value-using-class-d class instance slot) +unbound-slot-marker+)
  instance)