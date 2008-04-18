;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Persistent class t and slot meta objects

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
    :type persistent-class
    :documentation "The history class generated for this t class.")
   (t-value-slot
    (compute-as (find-slot (h-class-of -self-) 't-value))
    :type persistent-effective-slot-definition)
   (t-value-column
    (compute-as (first (columns-of (t-value-slot-of -self-))))
    :type column)
   (validity-start-slot
    (compute-as (find-slot (h-class-of -self-) 'validity-start))
    :type persistent-effective-slot-definition)
   (validity-start-column
    (compute-as (first (columns-of (validity-start-slot-of -self-))))
    :type column)
   (validity-end-slot
    (compute-as (find-slot (h-class-of -self-) 'validity-end))
    :type persistent-effective-slot-definition)
   (validity-end-column
    (compute-as (first (columns-of (validity-end-slot-of -self-))))
    :type column)
   (parent-slot
    (compute-as (find-slot (h-class-of -self-) 't-object))
    :type column)
   (parent-id-column
    (compute-as (id-column-of (parent-slot-of -self-)))
    :type column)
   (prefetched-slots
    (compute-as (collect-if #L(and (not (typep !1 'persistent-effective-slot-definition-t))
                                   (prefetch-p !1))
                            (persistent-effective-slots-of -self-)))))
  (:documentation "A temporal slot value is cached in the underlying slot. A time dependent slot value is cached as a values-having-validity object."))

(defcclass* persistent-slot-definition-t (persistent-slot-definition)
  ((time-dependent
    #f
    :type boolean)
   (temporal
    #f
    :type boolean)))

(defcclass* persistent-direct-slot-definition-t (persistent-slot-definition-t persistent-direct-slot-definition)
  ()
  (:metaclass identity-preserving-class))

(defcclass* persistent-effective-slot-definition-t (persistent-slot-definition-t persistent-effective-slot-definition)
  ((h-slot
    (compute-as (find-slot (h-class-of (slot-definition-class -self-)) (slot-definition-name -self-)))
    :type persistent-effective-slot-definition)))

(eval-always
  (mapc #L(pushnew !1 *allowed-slot-definition-properties*) '(:temporal :time-dependent)))

;;;;;;;;;;;;;
;;; defpclass

(defmethod expand-defpclass-form :around ((metaclass null) defclass-macro name superclasses slots options)
  (bind ((specified-metaclass (second (find :metaclass options :key #'first)))
         (processed-slots (mapcar 'ensure-list slots))
         (processed-options
          (if (and (not specified-metaclass)
                   (or (find :temporal processed-slots :test #'member)
                       (find :time-dependent processed-slots :test #'member)))
              (append options '((:metaclass persistent-class-t)))
              options)))
    (call-next-method metaclass defclass-macro name superclasses slots processed-options)))

(defmethod expand-defpclass-form ((metaclass persistent-class-t) defclass-macro name superclasses slots options)
  (bind ((t-class-name name)
         (h-class-name (t-class-name->h-class-name t-class-name))
         (processed-options (remove-if #L(member (first !1) '(:metaclass :slot-definition-transformer)) options))
         (h-superclasses
          (append (mapcar 't-class-name->h-class-name
                          (collect-if #L(awhen (find-class !1 nil)
                                          (typep it 'persistent-class-t))
                                      superclasses))
                  (when (find-if #L(find :temporal !1) slots)
                    (list 'temporal-object))
                  (when (find-if #L(find :time-dependent !1) slots)
                    (list 'time-dependent-object))
                  (list 'h-object))))
    `(progn
       ,(call-next-method)
       (,defclass-macro ,h-class-name ,h-superclasses
         ,(mapcar (lambda (slot-definition)
                    (bind ((slot-name (car slot-definition))
                           (slot-options (cdr slot-definition)))
                      (when (oddp (length slot-options)) ; remove initform from slot options
                        (pop slot-options))
                      (list* slot-name
                             +h-unused-slot-marker+
                             (aprog1 (remove-from-plistf slot-options :time-dependent :temporal)
                               (setf (getf it :type)
                                     (bind ((type (getf it :type)))
                                       (if (or-type-p type)
                                           `(or h-unused ,@(cdr type))
                                           `(or h-unused ,type))))))))
                  (collect-if #L(or (member :time-dependent !1)
                                    (member :temporal !1))
                              slots))
         (:metaclass ,(h-class-name->t-class-name (class-name (class-of metaclass))))
         ,@processed-options)
       (defassociation
         ((:class ,h-class-name :slot t-object :type ,t-class-name)
          (:class ,t-class-name :slot h-objects :type (set ,h-class-name))))
       (find-class ',name))))

;;;;;;;;;;;;
;;; Computed

(defmethod compute-columns ((slot persistent-effective-slot-definition-t))
  nil)

;;;;;;;;;;;
;;; Utility

(def function t-class-name->h-class-name (t-class-name)
  (concatenate-symbol t-class-name "-h"))

(def function h-class-name->t-class-name (h-class-name)
  (bind ((name (symbol-name h-class-name)))
    (intern (subseq name 0 (- (length name) 2)) (symbol-package h-class-name))))

(defmethod export-to-rdbms ((class persistent-class-t))
  (call-next-method)
  (export-to-rdbms (h-class-of class)))
