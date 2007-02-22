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
           (let ((initform (getf association-end :initform)))
             `(list ,@(mapcar #L`',!1 association-end)
               :initfunction
               (lambda ()
                 ,initform)))))
    (bind ((processed-association-ends (mapcar #'process-association-end (first association-ends)))
           (final-association-ends (cons 'list (mapcar #'add-initfunction processed-association-ends)))
           (primary-association-end (first processed-association-ends))
           (primary-class (getf primary-association-end :class))
           (primary-slot (getf primary-association-end :slot))
           (primary-reader (first (getf primary-association-end :readers)))
           (primary-writer (first (getf primary-association-end :writers)))
           (secondary-association-end (second processed-association-ends))
           (secondary-class (getf secondary-association-end :class))
           (secondary-slot (getf secondary-association-end :slot))
           (secondary-reader (first (getf secondary-association-end :readers)))
           (secondary-writer (first (getf secondary-association-end :writers)))
           (association-name (concatenate-symbol primary-class "-" primary-slot "-"
                                                 secondary-class "-" secondary-slot)))
      `(progn
        (eval-when (:compile-toplevel)
          (flet ((ensure-reader-function (name)
                   (ensure-generic-function name :lambda-list '(object)))
                 (ensure-writer-function (name)
                   (ensure-generic-function name :lambda-list '(new-value object))))
            (ensure-reader-function ',primary-reader)
            (ensure-reader-function ',(concatenate-symbol primary-reader "*"))
            (ensure-writer-function ',primary-writer)
            (ensure-reader-function ',secondary-reader)
            (ensure-reader-function ',(concatenate-symbol secondary-reader "*"))
            (ensure-writer-function ',secondary-writer)))
        (eval-when (:load-toplevel :execute)
          (flet ((ensure-persistent-class (name)
                   (bind ((class (find-class name)))
                     (ensure-class name
                                   :metaclass (class-of class)
                                   :direct-superclasses (class-direct-superclasses class)
                                   :direct-slots (mapcar
                                                  #L(list :instance !1)
                                                  (remove-if #L(typep !1 'persistent-association-end-direct-slot-definition)
                                                             (class-direct-slots class)))))))
            (prog1
                (aif (find-association ',association-name)
                     (reinitialize-instance it :association-end-definitions ,final-association-ends)
                     (setf (find-association ',association-name)
                           (make-instance 'persistent-association
                                          :name ',association-name
                                          :association-end-definitions ,final-association-ends)))
              (ensure-persistent-class ',primary-class)
              (ensure-persistent-class ',secondary-class))))))))

(defmacro defassociation* (&body association-ends)
  `(defassociation
    ,(mapcar #L(append !1
                       (unless (getf !1 :accessor)
                         `(:accessor ,(default-accessor-name-transformer (getf !1 :slot) nil)))
                       (unless (getf !1 :initarg)
                         `(:initarg ,(default-initarg-name-transformer (getf !1 :slot) nil))))
             (first association-ends))))

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

(defgeneric make-persistent (object)
  (:documentation "Makes an object persistent without making its associated objects persistent.")

  (:method :around (object)
           (unless (persistent-p object)
             (call-next-method))))

(defgeneric make-transient (object)
  (:documentation "Makes an object transient without making its associated objects transient.")

  (:method :around (object)
           (when (persistent-p object)
             (call-next-method))))

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

(defmacro with-lazy-collections (&body body)
  `(bind ((*lazy-collections* #t))
    ,@body))

(defmacro without-lazy-collections (&body body)
  `(bind ((*lazy-collections* #f))
    ,@body))

;;;;;;;;;;;;;;;;;;;
;;; database access

(defmacro with-bypassing-database-access (&body body)
  `(bind ((*bypass-database-access* #t))
    ,@body))

(defmacro without-bypassing-database-access (&body body)
  `(bind ((*bypass-database-access* #f))
    ,@body))
