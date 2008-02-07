;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(defmethod make-persistent-using-class :after ((class persistent-class-t) (instance persistent-object))
  (dolist (slot (persistent-effective-slot-ts-of class))
    (bind ((value (underlying-slot-boundp-or-value-using-class class instance slot)))
      (unless (unbound-slot-marker-p value)
        (if (values-having-validity-p value)
            (iter (for (value validity-start validity-end) :in-values-having-validity value)
                  (with-validity-range validity-start validity-end
                    (store-slot class instance slot value)))
            (store-slot class instance slot value))))))

(defmethod make-transient-using-class :after ((class persistent-class-t) (instance persistent-object))
  (error "Not yet implemented"))
