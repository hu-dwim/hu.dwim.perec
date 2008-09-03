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