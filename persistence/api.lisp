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
                                 (list* `(:metaclass ,metaclass) options))))))

(defmacro defpclass (name superclasses slots &rest options)
  "Defines a persistent class. Slots may have an additional :persistent slot option which is true by default. For standard options see defclass."
  (expand-defpclass-form nil 'defclass name superclasses slots options))

(defmacro defpclass* (name superclasses slots &rest options)
  "Same as defpclass but uses defclass*."
  (expand-defpclass-form nil 'defclass* name superclasses slots options))

;;;;;;;;;;;;;;;;;;
;;; defassociation

(defmacro defassociation (&body association-ends)
  (flet ((process-association-end (association-end)
           (bind ((initarg (getf association-end :initarg))
                  (accessor (getf association-end :accessor))
                  (reader (or (getf association-end :reader) accessor))
                  (writer (or (getf association-end :writer) `(setf ,accessor))))
             (append `(:readers (,reader)
                       :writers (,writer)
                       :initargs (,initarg))
                     association-end)))
         (add-initfunction (association-end)
           `(list ,@(mapcar #L`',!1 association-end)
             ,@(when (hasf association-end :initform)
                     `(:initfunction
                       (lambda ()
                         ,(getf association-end :initform)))))))
    (bind ((options (cdr association-ends))
           (metaclass (or (second (find :metaclass options :key #'first))
                          'persistent-association))
           (export-accessors-names-p (second (find :export-accessor-names-p options :key #'first)))
           (processed-association-ends (mapcar #'process-association-end (first association-ends)))
           (final-association-ends (cons 'list (mapcar #'add-initfunction processed-association-ends)))
           (primary-association-end (first processed-association-ends))
           (primary-class (getf primary-association-end :class))
           (primary-slot (getf primary-association-end :slot))
           (primary-reader (first (getf primary-association-end :readers)))
           (lazy-primary-reader (concatenate-symbol primary-reader "*"))
           (primary-writer (first (getf primary-association-end :writers)))
           (secondary-association-end (second processed-association-ends))
           (secondary-class (getf secondary-association-end :class))
           (secondary-slot (getf secondary-association-end :slot))
           (secondary-reader (first (getf secondary-association-end :readers)))
           (lazy-secondary-reader (concatenate-symbol secondary-reader "*"))
           (secondary-writer (first (getf secondary-association-end :writers)))
           (association-name (concatenate-symbol primary-class "~" primary-slot "~"
                                                 secondary-class "~" secondary-slot)))
      `(progn
        (eval-when (:compile-toplevel)
          (flet ((ensure-reader-function (name)
                   (ensure-generic-function name :lambda-list '(instance)))
                 (ensure-writer-function (name)
                   (ensure-generic-function name :lambda-list '(new-value instance))))
            (ensure-reader-function ',primary-reader)
            (ensure-reader-function ',lazy-primary-reader)
            (ensure-writer-function ',primary-writer)
            (ensure-reader-function ',secondary-reader)
            (ensure-reader-function ',lazy-secondary-reader)
            (ensure-writer-function ',secondary-writer)))
        (eval-when (:load-toplevel :execute)
          (flet ((ensure-persistent-class (name)
                   (bind ((class (find-class name)))
                     (ensure-class name
                                   :metaclass (class-of class)
                                   ;; TODO: what about killing other class options?
                                   :abstract (list (abstract-p class))
                                   :direct-superclasses (class-direct-superclasses class)
                                   :direct-slots (mapcar
                                                  #L(list :instance !1)
                                                  (remove-if #L(typep !1 'persistent-association-end-direct-slot-definition)
                                                             (class-direct-slots class)))))))
            (prog1
                (aif (find-association ',association-name)
                     (reinitialize-instance it :association-end-definitions ,final-association-ends)
                     (setf (find-association ',association-name)
                           (make-instance ',metaclass
                                          :name ',association-name
                                          :association-end-definitions ,final-association-ends)))
              (ensure-persistent-class ',primary-class)
              (ensure-persistent-class ',secondary-class))))
        ,(when export-accessors-names-p
               `(export '(,primary-reader ,lazy-primary-reader ,secondary-reader ,lazy-secondary-reader)
                 ,*package*))
        ',association-name))))

(defmacro defassociation* (&body association-ends)
  `(defassociation
    ,(mapcar #L(append !1
                       (unless (getf !1 :accessor)
                         `(:accessor ,(default-accessor-name-transformer (getf !1 :slot) nil)))
                       (unless (getf !1 :initarg)
                         `(:initarg ,(default-initarg-name-transformer (getf !1 :slot) nil))))
             (first association-ends))
    ,@(cdr association-ends)))

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

;;; insert-item, delete-item, empty-p, empty!, search-for-item are inherited from cl-containers

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
