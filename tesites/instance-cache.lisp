;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(def (function o) cache-to-many-association-ends-t (instances children-slot-provider parent-accessor)
  (dolist (instance instances)
    (bind ((class (class-of instance))
           (children-slot (funcall children-slot-provider class)))
      (when (null (underlying-slot-boundp-or-value-using-class class instance children-slot))
        (setf (underlying-slot-boundp-or-value-using-class class instance children-slot)
              (make-single-values-having-validity nil *validity-start* *validity-end*)))
      (when-bind parent (funcall parent-accessor instance)
       (bind ((parent-class (class-of parent))
              (parent-children-slot (funcall children-slot-provider parent-class))
              ((:values cached-p children)
               (slot-value-cached-p parent parent-children-slot)))
         (assert cached-p)
         (setf (underlying-slot-boundp-or-value-using-class parent-class parent parent-children-slot)
               (make-single-values-having-validity
                (cons instance (single-values-having-validity-value children))
                *validity-start* *validity-end*)))))))

(def (function o) cache-to-many-association-ends-for-tree-t (instances children-slot-provider parent-accessor)
  (dolist (instance instances)
    (bind ((class (class-of instance))
           (children-slot (funcall children-slot-provider class)))
      (setf (underlying-slot-boundp-or-value-using-class class instance children-slot) nil)))
  (cache-to-many-association-ends-t instances children-slot-provider parent-accessor)
  (find-tree-root (first instances) parent-accessor))

(def macro ensure-cached-to-many-association-ends-for-tree-t (bulk instances children-slot-provider parent-accessor)
  `(aif (cached-bulk-of ,bulk)
    it
    (setf (cached-bulk-of ,bulk)
     (cache-to-many-association-ends-for-tree-t ,instances ,children-slot-provider ,parent-accessor))))

;;
;;
;;
(def function set-cached-slot-values-from-h-instances (h-instances)
  ""
  (iter (for h-instance :in h-instances)
        (for t-instance = (t-object-of h-instance))
        (for t-class = (class-of t-instance))
        (for prev-t-instance previous t-instance)
        (for t-value = (when (typep h-instance 'temporal-object) (t-value-of h-instance)))
        (for prev-t-value previous t-value)
        (for validity-start = (when (typep h-instance 'time-dependent-object)
                                (timestamp-maximum (validity-start-of h-instance) *validity-start*)))
        (for validity-end = (when (typep h-instance 'time-dependent-object)
                              (timestamp-minimum (validity-end-of h-instance) *validity-end*)))
        (assert (or (null prev-t-value) (and t-value (timestamp<= prev-t-value t-value))))
        (iter (for slot :in (class-slots t-class))
              (unless (typep slot 'persistent-slot-definition-t) (next-iteration))
              (when (typep slot 'persistent-association-end-slot-definition) (next-iteration))
              (for slot-name = (slot-definition-name slot))
              (for slot-value = (if (slot-boundp h-instance slot-name)
                                    (slot-value h-instance slot-name)
                                    +unbound-slot-marker+))
              (when (eq slot-value +h-unused-slot-marker+) (next-iteration))
              (if (time-dependent-p slot)
                  (bind (((:values cached-p t-slot-value) (slot-value-cached-p t-instance slot)))
                    (unless cached-p
                      (setf t-slot-value
                            (make-empty-values-having-validity)
                            (underlying-slot-boundp-or-value-using-class t-class t-instance slot)
                            t-slot-value))
                    (setf (values-having-validity-value t-slot-value validity-start validity-end)
                          slot-value))
                  (setf (underlying-slot-boundp-or-value-using-class t-class t-instance slot) slot-value)))))

