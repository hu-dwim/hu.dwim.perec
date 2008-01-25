;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(defcclass* persistent-class-t (persistent-class)
  ((persistent-effective-slot-ts
    (compute-as (collect-if #L(typep !1 'persistent-effective-slot-definition-t) (standard-effective-slots-of -self-)))
    :type (list persistent-effective-slot-definition-t))
   (effective-slots-with-underlying-slot-access
    (compute-as (append (persistent-effective-slots-of -self-) (persistent-effective-slot-ts-of -self-)))
    :type (list standard-effective-slot-definition)
    :documentation "A list of slots that support the underlying-slot-value protocol.")
   (h-class
    (compute-as (find-class (t-class-name->h-class-name (class-name -self-))))
    :type persistent-class)
   (t-value-slot
    (compute-as (find-slot (h-class-of -self-) 't-value)))
   (t-value-column
    (compute-as (first (columns-of (t-value-slot-of -self-))))
    :type column)
   (validity-start-slot
    (compute-as (find-slot (h-class-of -self-) 'validity-start)))
   (validity-start-column
    (compute-as (first (columns-of (validity-start-slot-of -self-))))
    :type column)
   (validity-end-slot
    (compute-as (find-slot (h-class-of -self-) 'validity-end)))
   (validity-end-column
    (compute-as (first (columns-of (validity-end-slot-of -self-))))
    :type column)
   (parent-slot
    (compute-as (find-slot (h-class-of -self-) 't-object))
    :type column)
   (parent-id-column
    (compute-as (id-column-of (parent-slot-of -self-)))
    :type column))
  (:documentation "A temporal slot value is cached in the underlying slot. A time dependent slot value is cached as a values-having-validity object."))

(defcclass* persistent-slot-definition-t (standard-slot-definition)
  ((persistent
    ;; TODO: remove this slot and fix mop
    )
   (prefetch
    :type boolean
    :computed-in compute-as
    :documentation "Prefetched slots are loaded from and stored into the database at once. A prefetched slot must be in a table which can be accessed using a where clause matching to the id of the instance thus it must be in a data table. The default prefetched slot semantics can be overriden on a per direct slot basis.")
   (cache
    :type boolean
    :computed-in compute-as
    :documentation "All prefetched slots are cached slots but the opposite may not be true. When a cached slot is loaded it's value will be stored in the CLOS instance for fast subsequent read operations. Also whenever a cached slot is set the value will be remembered. The default cached slot semantics can be overriden on a per direct slot basis.")
   (time-dependent
    #f
    :type boolean)
   (temporal
    #f
    :type boolean)
   (integrated-slot-name
    nil
    :type symbol)))

(defcclass* persistent-direct-slot-definition-t
    (persistent-slot-definition-t standard-direct-slot-definition)
  ()
  (:metaclass identity-preserving-class))

(defcclass* persistent-effective-slot-definition-t
    (persistent-slot-definition-t standard-effective-slot-definition)
  ((h-slot
    (compute-as (find-slot (h-class-of (slot-definition-class -self-)) (slot-definition-name -self-)))
    :type persistent-effective-slot-definition)
   (prefetch
    (compute-as (prefetch-p (h-slot-of -self-)))
    :documentation "The cached option is inherited from the corresponding h slot.")
   (cache
    (compute-as (cache-p (h-slot-of -self-)))
    :documentation "The cached option is inherited from the corresponding h slot.")))

(eval-always
  ;; TODO: kill association?
  (mapc #L(pushnew !1 *allowed-slot-definition-properties*) '(:temporal :time-dependent :integrated-slot-name :association)))

(defun t-class-name->h-class-name (class-name)
  (concatenate-symbol class-name "-h"))

(defun h-class-name->t-class-name (t-class-name)
  (bind ((name (symbol-name t-class-name)))
    (intern (subseq name 0 (- (length name) 2)) (symbol-package t-class-name))))

(defmethod export-to-rdbms ((class persistent-class-t))
  (call-next-method)
  (export-to-rdbms (h-class-of class)))
