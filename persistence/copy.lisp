;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(define-copy-protocol copy-into-transaction-cache)

(define-copy-method (copy-self copy-into-transaction-cache)
    ((instance persistent-object))
  (prc::make-revived-instance (class-of instance) :oid (prc::oid-of instance)))

(define-copy-method (copy-self copy-into-transaction-cache)
    ((instance local-time))
  ;; TODO: new instance
  instance)

(define-copy-method (copy-inner-class copy-into-transaction-cache) progn
  ((old persistent-object) (new persistent-object) hash-table)
  (bind ((class (class-of old)))
    (dolist (slot (closer-mop:class-slots class))
      (cond ((typep slot 'prc::persistent-effective-slot-definition)
             (bind (((values cached-p value) (prc::slot-value-cached-p old slot)))
               (when cached-p
                 (setf (prc::underlying-slot-boundp-or-value-using-class class new slot)
                       (if (prc::unbound-marker-p value)
                           value
                           (copy-one value hash-table))))))
            ((typep slot 'closer-mop:standard-effective-slot-definition)
             (setf (closer-mop:standard-instance-access new (closer-mop:slot-definition-location slot))
                   (closer-mop:standard-instance-access old (closer-mop:slot-definition-location slot))))))))

(define-copy-method (copy-inner-class copy-into-transaction-cache) progn
  ((old local-time) (new local-time) hash-table))
