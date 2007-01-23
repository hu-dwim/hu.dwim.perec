;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;;;;;;;
;;; defpclass

(defmacro defpclass (name superclasses slots &rest options)
  "Defines a persistent class. Slots may have an additional :persistent slot option which is true by default. For standard options see defclass."
  `(defclass ,name ,superclasses , slots
    ,@(append (unless (find :metaclass options :key 'first)
                '((:metaclass persistent-class)))
              options)))

(defmacro defpclass* (name superclasses slots &rest options)
  "Same as defpclass but uses defclass*."
  `(defclass* ,name ,superclasses , slots
    ,@(append (unless (find :metaclass options :key 'first)
                '((:metaclass persistent-class)))
              options)))

;; :persistent is a slot definition option and may be set to #t or #f
(eval-always
  (pushnew :persistent *allowed-slot-definition-properties*))

;;;;;;;;;;;;;;;;;;
;;; defassociation

(defmacro defassociation (&body body)
  (declare (ignore body))
  nil)

(defmacro defassociation* (&body body)
  (declare (ignore body))
  nil)

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

(defgeneric make-persistent (transient-object)
  (:documentation "Makes an object persistent without making its associated objects persistent."))

(defgeneric make-transient (persistent-object)
  (:documentation "Makes an object transient without making its associated objects transient."))

;;;;;;;;;;;;;;
;;; collection

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

(defmacro with-bypassing-database-access (&body body)
  `(bind ((*bypass-database-access* #t))
    ,@body))

(defmacro without-bypassing-database-access (&body body)
  `(bind ((*bypass-database-access* #f))
    ,@body))
