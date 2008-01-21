;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(defmethod make-persistent-using-class :after ((class persistent-class-t) (instance persistent-object))
  (dolist (slot (persistent-effective-slot-ts-of class))
    (bind ((values-having-validity (underlying-slot-boundp-or-value-using-class class instance slot)))
      (unless (unbound-marker-p values-having-validity)
        (iter (for (value validity-start validity-end) :in-values-having-validity values-having-validity)
              (with-validity-range validity-start validity-end
                (store-slot-t class instance slot value)))))))

(defmethod make-transient-using-class :after ((class persistent-class-t) (instance persistent-object))
  (error "Not yet implemented"))
