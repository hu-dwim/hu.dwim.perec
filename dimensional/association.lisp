;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Persistent association t and slot meta objects

(defcclass* persistent-association-d (persistent-association)
  ((h-class
    (compute-as (find-class (name-of -self-)))
    :type persistent-class)))

(def method compute-primary-table ((association persistent-association-d) current-table)
  nil)

(defcclass* persistent-association-end-slot-definition-d (persistent-slot-definition-d persistent-association-end-slot-definition)
  ())

(defcclass* persistent-association-end-direct-slot-definition-d
    (persistent-association-end-slot-definition-d persistent-direct-slot-definition-d persistent-association-end-direct-slot-definition)
  ()
  (:metaclass identity-preserving-class))

(defcclass* persistent-association-end-effective-slot-definition-d
    (persistent-association-end-slot-definition-d persistent-effective-slot-definition-d persistent-association-end-effective-slot-definition)
  ((prefetch #f)          ;; TODO temporarily
   (type-check :always)
   (cache #f)             ;; TODO temporarily
   (h-class
    (compute-as (h-class-of (association-of -self-)))
    :type persistent-class)
   (table
    (compute-as (primary-table-of (h-class-of -self-)))
    :type table)
   (action-slot
    (compute-as (find-persistent-slot (h-class-of -self-) 'action :otherwise nil))
    :type persistent-effective-slot-definition)
   (h-slot-name
    (compute-as (concatenate-symbol "d-" (slot-definition-name (other-association-end-of -self-))))
    :type symbol)
   (h-slot
    (compute-as (find-persistent-slot (h-class-of -self-) (h-slot-name-of -self-) :otherwise nil))
    :type persistent-effective-slot-definition)
   (other-end-h-slot
    (compute-as (h-slot-of (other-association-end-of -self-)))
    :type list)))

;;;;;;;;;;;;;;;;;;
;;; defassociation

(def method expand-defassociation-form :around ((metaclass null) association-ends options)
  (bind ((specified-metaclass (second (find :metaclass options :key #'first)))
         (processed-options
          (if (and (not specified-metaclass)
                   (or (find :dimensions options :key #'first)))
              (append options '((:metaclass persistent-association-d)))
              options)))
    (call-next-method metaclass association-ends processed-options)))

(def method expand-defassociation-form ((metaclass persistent-association-d) association-ends options)
  (with-decoded-association-ends association-ends
    (bind ((dimensions (second (find :dimensions options :key #'first)))
           (superclasses
            (iter (for dimension :in dimensions)
                  (adjoining (dependent-object-name dimension))))
           (processed-options (remove-if #L(member (first !1) '(:temporal :time-dependent)) options))
           (processed-association-ends
            (mapcar #L(append (when dimensions
                                (list :dimensions dimensions))
                              !1)
                    association-ends))
           (primary-slot-type (getf primary-association-end :type))
           (secondary-slot-type (getf secondary-association-end :type))
           (slot-definitions (when (or (set-type-p* primary-slot-type)
                                       (set-type-p* secondary-slot-type))
                               '((action :accessor action-of :type integer-8)))))
      `(progn
         ,(call-next-method metaclass processed-association-ends processed-options)
         (defpclass* ,association-name ,superclasses
           ,slot-definitions)
         (defassociation*
           ((:class ,association-name :slot ,(concatenate-symbol "d-" secondary-slot *package*) :type (or null ,primary-class))
            (:class ,primary-class :slot ,(concatenate-symbol "h-" primary-slot *package*) :type (set ,association-name))))
         (defassociation*
           ((:class ,association-name :slot ,(concatenate-symbol "d-" primary-slot *package*) :type (or null ,secondary-class))
            (:class ,secondary-class :slot ,(concatenate-symbol "h-" secondary-slot *package*) :type (set ,association-name))))))))
