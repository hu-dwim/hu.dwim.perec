;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Persistent class t and slot meta objects

(defcclass* persistent-class-d (persistent-class)
  ((persistent-effective-slot-ds
    (compute-as (collect-if #L(typep !1 'persistent-effective-slot-definition-d) (standard-effective-slots-of -self-)))
    :type (list persistent-effective-slot-definition-d))
   (effective-slots-with-underlying-slot-access
    (compute-as (append (persistent-effective-slots-of -self-) (persistent-effective-slot-ds-of -self-)))
    :type (list standard-effective-slot-definition)
    :documentation "A list of slots that support the underlying-slot-value protocol.")
   (h-class
    (compute-as (find-class (d-class-name->h-class-name (class-name -self-))))
    :type persistent-class
    :documentation "The history class generated for this t class.")
   (dimensional-h-slots
    (compute-as (compute-dimensional-h-slots-of -self-))
    :type (list (cons dimension (list slot-definition))))
   (dimensional-h-columns
    (compute-as (mapcar [cons !1 (mapcan #'columns-of !2)] (dimensional-h-slots-of -self-)))
    :type (list (cons dimension column)))
   (parent-slot
    (compute-as (find-persistent-slot (h-class-of -self-) 'd-object :otherwise nil))
    :type column)
   (parent-id-column
    (compute-as (id-column-of (parent-slot-of -self-)))
    :type column)
   (prefetched-slots
    (compute-as (collect-if #L(and (not (typep !1 'persistent-effective-slot-definition-d))
                                   (prefetch-p !1))
                            (persistent-effective-slots-of -self-)))))
  (:documentation "A temporal slot value is cached in the underlying slot. A time dependent slot value is cached as a values-having-validity object."))

(def function h-slots-of (class dimension)
  (cdr (assoc dimension (dimensional-h-slots-of class))))

(def function h-slot-of (class dimension)
  (bind ((h-slots (h-slots-of class dimension)))
    (assert (length= 1 h-slots))
    (first h-slots)))

(def function h-readers-of (class dimension)
  (mapcar
   (lambda (slot)
     (aprog1 (first (slot-definition-readers slot))
       (assert it)))
   (h-slots-of class dimension)))

(def function h-reader-of (class dimension)
  (aprog1 (first (slot-definition-readers (h-slot-of class dimension)))
    (assert it)))

(def function h-begin-reader-of (class dimension)
  (assert (and (ordered-p dimension) (null (inherit-of dimension))))
  (first (h-readers-of class dimension)))

(def function h-end-reader-of (class dimension)
  (assert (and (ordered-p dimension) (null (inherit-of dimension))))
  (second (h-readers-of class dimension)))

(defcclass* persistent-slot-definition-d (persistent-slot-definition)
  ((dimensions
    '()
    :type (list dimension))))

(defcclass* persistent-direct-slot-definition-d (persistent-slot-definition-d persistent-direct-slot-definition)
  ()
  (:metaclass identity-preserving-class))

(defcclass* persistent-effective-slot-definition-d (persistent-slot-definition-d persistent-effective-slot-definition)
  ((h-slot
    (compute-as (find-persistent-slot (h-class-of (slot-definition-class -self-)) (slot-definition-name -self-) :otherwise nil))
    :type persistent-effective-slot-definition)))

(eval-always
  (pushnew :dimensions *allowed-slot-definition-properties*))

;;;;;;;;;;;;;
;;; defpclass

(defmethod expand-defpclass-form :around ((metaclass null) defclass-macro name superclasses slots options)
  (bind ((specified-metaclass (second (find :metaclass options :key #'first)))
         (processed-slots (mapcar 'ensure-list slots))
         (processed-options
          (if (and (not specified-metaclass)
                   (or (find :dimensions processed-slots :test #'member)))
              (append options '((:metaclass persistent-class-d)))
              options)))
    (call-next-method metaclass defclass-macro name superclasses slots processed-options)))

(defmethod expand-defpclass-form ((metaclass persistent-class-d) defclass-macro name superclasses slots options)
  (flet ((slot-options-of (slot-definition)
           (if (oddp slot-definition)
               (cdr slot-definitiion)   ; no init-form
               (cddr slot-definition))))
    (bind ((d-class-name name)
           (h-class-name (d-class-name->h-class-name d-class-name))
           (processed-options (remove-if #L(member (first !1) '(:metaclass :slot-definition-transformer)) options))
           (h-superclasses
            (append (mapcar 'd-class-name->h-class-name
                            (collect-if #L(awhen (find-class !1 nil)
                                            (typep it 'persistent-class-d))
                                        superclasses))
                    (iter outer
                          (for slot-definition :in slots)
                          (for slot-options = (slot-options-of slot-definition))
                          (for dimensions = (getf slot-options :dimensions))
                          (iter (for dimension :in dimensions)
                                (in outer (adjoining
                                           (dependent-object-of
                                            (find-dimension dimension :otherwise error))))))
                    (list 'h-object))))
      `(progn
         ,(call-next-method)
         (,defclass-macro ,h-class-name ,h-superclasses
           ,(mapcar (lambda (slot-definition)
                      (bind ((slot-name (car slot-definition))
                             (slot-options (remove-from-plist (slot-options-of slot-definition)
                                                              :dimensions))
                             (type (getf slot-options :type)))
                        (setf (getf slot-options :type)
                              (if (or-type-p type)
                                  `(or h-unused ,@(cdr type))
                                  `(or h-unused ,type)))
                        (list* slot-name
                               +h-unused-slot-marker+
                               slot-options)))
                    (collect-if [getf (slot-options-of !1) :dimensions] slots))
           (:metaclass ,(h-class-name->d-class-name (class-name (class-of metaclass)))) ; FIXME ???
           ,@processed-options)
         (defassociation
           ((:class ,h-class-name :slot d-object :type ,d-class-name)
            (:class ,d-class-name :slot h-objects :type (set ,h-class-name))))
         (find-class ',name)))))

;;;;;;;;;;;;
;;; Computed

(defmethod compute-columns ((slot persistent-effective-slot-definition-d))
  nil)

(def function compute-dimensional-h-slots-of (class)
  (declare (ignore class))
  ;TODO
  )

;;;;;;;;;;;
;;; Utility

(def function d-class-name->h-class-name (t-class-name)
  (concatenate-symbol t-class-name "-h"))

(def function h-class-name->d-class-name (h-class-name)
  (bind ((name (symbol-name h-class-name)))
    (intern (subseq name 0 (- (length name) 2)) (symbol-package h-class-name))))

(defmethod export-to-rdbms ((class persistent-class-d))
  (call-next-method)
  (export-to-rdbms (h-class-of class)))
