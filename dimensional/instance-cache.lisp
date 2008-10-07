;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;
;;; Association end cache

(def (function o) cache-to-many-association-ends-d (instances children-slot-provider parent-accessor)
  (dolist (instance instances)
    (bind ((class (class-of instance))
           (children-slot (funcall children-slot-provider class)))
      (when (null (underlying-slot-boundp-or-value-using-class class instance children-slot))
        (setf (underlying-slot-boundp-or-value-using-class class instance children-slot)
              (make-single-d-value nil *validity-begin* *validity-end*)))
      (when-bind parent (funcall parent-accessor instance)
       (bind ((parent-class (class-of parent))
              (parent-children-slot (funcall children-slot-provider parent-class))
              ((:values cached-p children)
               (slot-value-cached-p parent parent-children-slot)))
         (assert cached-p)
         (setf (underlying-slot-boundp-or-value-using-class parent-class parent parent-children-slot)
               (make-single-d-value
                (cons instance (single-d-value children))
                *validity-begin* *validity-end*)))))))

(def (function o) cache-to-many-association-ends-for-tree-d (instances children-slot-provider parent-accessor)
  (dolist (instance instances)
    (bind ((class (class-of instance))
           (children-slot (funcall children-slot-provider class)))
      (setf (underlying-slot-boundp-or-value-using-class class instance children-slot) nil)))
  (cache-to-many-association-ends-d instances children-slot-provider parent-accessor)
  (find-tree-root (first instances) parent-accessor))

(def macro ensure-cached-to-many-association-ends-for-tree-d (bulk instances children-slot-provider parent-accessor)
  `(aif (cached-bulk-of ,bulk)
    it
    (setf (cached-bulk-of ,bulk)
     (cache-to-many-association-ends-for-tree-d ,instances ,children-slot-provider ,parent-accessor))))

;;;;;;
;;; Cache multiple slot values

(def function set-cached-slot-values-from-h-instances (h-instances)
  (iter (for h-instance :in h-instances)
        (for d-instance = (d-instance-of h-instance))
        (for d-class = (class-of d-instance))
        (for prev-d-instance previous d-instance)
        (for t-value = (when (typep h-instance 'time-dependent-object) (time-of h-instance)))
        (for prev-d-value previous t-value)
        (for validity-begin = (when (typep h-instance 'validity-dependent-object)
                                (timestamp-maximum (validity-begin-of h-instance) *validity-begin*)))
        (for validity-end = (when (typep h-instance 'validity-dependent-object)
                              (timestamp-minimum (validity-end-of h-instance) *validity-end*)))
        (assert (or (null prev-d-value) (and t-value (timestamp<= prev-d-value t-value))))
        (iter (for slot :in (class-slots d-class))
              (unless (typep slot 'persistent-slot-definition-d) (next-iteration))
              (when (typep slot 'persistent-association-end-slot-definition) (next-iteration))
              (for slot-name = (slot-definition-name slot))
              (for slot-value = (if (slot-boundp h-instance slot-name)
                                    (slot-value h-instance slot-name)
                                    +unbound-slot-marker+))
              (when (eq slot-value +h-unused-slot-marker+) (next-iteration))
              #+nil
              (if (time-dependent-p slot)
                  (bind (((:values cached-p d-slot-value) (slot-value-cached-p d-instance slot)))
                    (unless cached-p
                      (setf d-slot-value
                            (make-empty-d-value)
                            (underlying-slot-boundp-or-value-using-class d-class d-instance slot)
                            d-slot-value))
                    (setf (value-at-coordinates d-slot-value validity-begin validity-end)
                          slot-value))
                  (setf (underlying-slot-boundp-or-value-using-class d-class d-instance slot) slot-value)))))
