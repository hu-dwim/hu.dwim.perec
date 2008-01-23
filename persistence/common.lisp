;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(defparameter *exporting-to-rdbms* #f)

(defcclass* exportable ()
  ((ensure-exported
    (compute-as (export-to-rdbms -self-) -self-)
    :reader ensure-exported
    :documentation "A persistent class, a persistent association and the related tables must be exported before use. This will automatically happen not later than making, reviving, querying or using by any means the first instance of it.")))

(defgeneric export-to-rdbms (instance)
  (:documentation "Exports classes, associations, tables to the database, may create new tables or alter existing ones.")

  (:method :around (instance)
           (if *exporting-to-rdbms*
               (call-next-method)
               (let ((*exporting-to-rdbms* #t))
                 (with-transaction
                   (call-next-method))))))

(def function ensure-all-computed-slots-are-valid (thing)
  (bind ((class (class-of thing)))
    (dolist (slot (class-slots class))
      (when (typep slot 'cc::computed-effective-slot-definition)
        ;; KLUDGE: FIXME: please!
        (ignore-errors
          (slot-value-using-class class thing slot))))))


