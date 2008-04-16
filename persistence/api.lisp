;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;;;;;;;
;;; defpclass

(defgeneric expand-defpclass-form (metaclass defclass-macro name superclasses slots options)
  (:method ((metaclass null) defclass-macro name superclasses slots options)
    (bind ((specified-metaclass (second (find :metaclass options :key #'first)))
           (metaclass (or specified-metaclass 'persistent-class)))
      (expand-defpclass-form (class-prototype (find-class metaclass))
                             defclass-macro name superclasses slots
                             (if specified-metaclass
                                 options
                                 (append options `((:metaclass ,metaclass))))))))

(defmacro defpclass (name superclasses slots &rest options)
  "Defines a persistent class. Slots may have an additional :persistent slot option which is true by default. For standard options see defclass."
  (expand-defpclass-form nil 'defclass name superclasses slots options))

(defmacro defpclass* (name superclasses slots &rest options)
  "Same as defpclass but uses defclass*."
  (expand-defpclass-form nil 'defclass* name superclasses slots options))

;;;;;;;;;;;;;;;;;;
;;; defassociation

(defgeneric expand-defassociation-form (metaclass association-ends options)
  (:method ((metaclass null) association-ends options)
    (bind ((specified-metaclass (second (find :metaclass options :key #'first)))
           (metaclass (or specified-metaclass 'persistent-association)))
      (expand-defassociation-form (class-prototype (find-class metaclass))
                                  association-ends
                                  (if specified-metaclass
                                      options
                                      (append options `((:metaclass ,metaclass))))))))

(defmacro defassociation (&body association-ends)
  (expand-defassociation-form nil (car association-ends) (cdr association-ends)))

(defmacro defassociation* (&body association-ends)
  (expand-defassociation-form nil
                              (mapcar #L(append !1
                                                (unless (getf !1 :accessor)
                                                  `(:accessor ,(default-accessor-name-transformer (getf !1 :slot) nil)))
                                                (unless (getf !1 :initarg)
                                                  `(:initarg ,(default-initarg-name-transformer (getf !1 :slot) nil))))
                                      (car association-ends))
                              (cdr association-ends)))

;;;;;;;;;
;;; types

;;; see types.lisp

;;;;;;;;;;;;;;;;;;;;
;;; with-transaction

;;; inherited from cl-rdbms

;;;;;;;;;;;;;;;;;
;;; with-database

;;; inherited from cl-rdbms

;;;;;;;;;;;;;;;
;;; persistence

(defun ensure-persistent (instance)
  (unless (persistent-p instance)
    (make-persistent instance)))

(defun ensure-transient (instance)
  (when (persistent-p instance)
    (make-transient instance)))

(defun make-persistent (instance)
  "Makes an instance persistent without making its associated instances persistent."
  (if (persistent-p instance)
      (error "Instance ~A is already persistent, you may want to use ensure-persistent instead" instance)
      (make-persistent-using-class (class-of instance) instance)))

(defun make-transient (instance)
  "Makes an instance transient without making its associated instances transient."
  (if (persistent-p instance)
      (make-transient-using-class (class-of instance) instance)
      (error "Instance ~A is already transient, you may want to use ensure-transient instead" instance)))

(defgeneric make-persistent-using-class (class instance)
  (:documentation "Extension point"))

(defgeneric make-transient-using-class (class instance)
  (:documentation "Extension point"))

;;;;;;;;;;;;;;
;;; collection

;;; insert-item, delete-item, empty-p, empty!, find-item are inherited from cl-containers

(defgeneric ensure-item (persistent-collection fn)
  (:documentation "Ensure that item is present in the container."))

(defgeneric iterate-items (persistent-collection fn)
  (:documentation "Applies function to each item in the persistent container."))

(defgeneric list-of (persistent-collection)
  (:documentation "Returns a non lazy list of items present in the persistent collection."))

(defgeneric (setf list-of) (new-value persistent-collection)
  (:documentation "Returns a non lazy list of items present in the persistent collection."))

;;;;;;;;;
;;; cache

(defmacro with-caching-slot-values (&body body)
  `(bind ((*cache-slot-values* #t))
    ,@body))

(defmacro without-caching-slot-values (&body body)
  `(bind ((*cache-slot-values* #f))
    ,@body))

;;;;;;;;;;;;
;;; laziness

(defmacro with-lazy-slot-value-collections (&body body)
  `(bind ((*lazy-slot-value-collections* #t))
    ,@body))

(defmacro without-lazy-slot-value-collections (&body body)
  `(bind ((*lazy-slot-value-collections* #f))
    ,@body))
