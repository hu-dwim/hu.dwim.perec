;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;
;;; Association end cache

(def function cache-to-many-association-ends-d (instances children-slot-provider parent-accessor)
  (dolist (instance instances)
    (bind ((class (class-of instance))
           (children-slot (funcall children-slot-provider class))
           (dimensions (dimensions-of children-slot))
           (coordinates (collect-coordinates-from-variables dimensions)))
      (when (null (underlying-slot-boundp-or-value-using-class class instance children-slot))
        (setf (underlying-slot-boundp-or-value-using-class class instance children-slot)
              (make-single-d-value dimensions coordinates nil)))
      (when-bind parent (funcall parent-accessor instance)
       (bind ((parent-class (class-of parent))
              (parent-children-slot (funcall children-slot-provider parent-class))
              ((:values cached-p cached-children)
               (slot-value-cached-p parent parent-children-slot)))
         (assert cached-p)
         (setf (underlying-slot-boundp-or-value-using-class parent-class parent parent-children-slot)
               (make-single-d-value
                dimensions
                coordinates
                (cons instance (single-d-value cached-children)))))))))

(def function cache-to-many-association-ends-for-tree-d (instances children-slot-provider parent-accessor)
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
        #| TODO
        (for inheriting-coordinate = (when inheriting-dimension
                                       (coordinate-range-begin
                                        (get-coordinate-from-h-instance h-instance inheriting-dimension))))
        (for prev-inheriting-coordinate :previous inheriting-coordinate)
        ;; check that h-instances are ordered according to the inheriting dimension
        (assert (or (null inheriting-coordinate)
                    (and inheriting-coordinate (coordinate<= prev-inheriting-coordinate
                                                             inheriting-coordinate))))
        |#
        (iter (for slot :in (dimensional-slots-of (class-of d-instance)))
              (unless (typep slot 'persistent-association-end-slot-definition)
                (set-cached-slot-value-from-h-instance d-instance h-instance (slot-definition-name slot))))))

(def function set-cached-slot-value-from-h-instance (d-instance h-instance slot-name)
  (bind ((slot-value (if (slot-boundp h-instance slot-name) (slot-value h-instance slot-name) +unbound-slot-marker+)))
    (unless (eq slot-value +h-unused-slot-marker+)
      (bind ((d-class (class-of d-instance))
             (d-slot (find-slot d-class slot-name))
             (dimensions (dimensions-of d-slot))
             (coordinates (coordinates-intersection
                           (get-coordinates-from-h-instance h-instance dimensions)
                           (collect-coordinates-from-variables dimensions)))
             ((:values cached-p cached-slot-value) (slot-value-cached-p d-instance d-slot)))
        (if cached-p
            (progn
              (assert (d-value-p cached-slot-value))
              (setf (value-at-coordinates cached-slot-value coordinates) slot-value))
            (setf (underlying-slot-boundp-or-value-using-class d-class d-instance d-slot)
                  (make-single-d-value dimensions coordinates slot-value)))))))

(def (function io) get-coordinates-from-h-instance (h-instance dimensions)
  (mapcar [get-coordinate-from-h-instance h-instance !1] dimensions))

(def function get-coordinate-from-h-instance (h-instance dimension)
  (etypecase dimension
    (inheriting-dimension
     (make-empty-coordinate-range
      (slot-value h-instance (slot-name-of dimension))))
    (ordering-dimension
     (make-ie-coordinate-range
      (slot-value h-instance (begin-slot-name-of dimension))
      (slot-value h-instance (end-slot-name-of dimension))))
    (dimension
     (bind ((value (slot-value h-instance (slot-name-of dimension))))
       (if value
           (list value)
           +whole-domain-marker+)))))
