;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(eval-always
  (unless (fboundp 'make-h-unused-slot-marker)
    (defstruct h-unused-slot-marker
      "This structure is used to mark unused slot values for persistent slots. The type for that marker must be a subtype of t and cannot be a subtype of any other type.")))

(def (constant :test equalp) +h-unused-slot-marker+ (make-h-unused-slot-marker)
  "This value is used to signal unbound slot value returned from database.")

(defmethod make-load-form ((instance h-unused-slot-marker) &optional environment)
  (declare (ignore environment))
  '+h-unused-slot-marker+)

;;;;;;;;;;;;
;;; h-unused
;;; 
;;; h-unused -> NULL
;;; t -> type-error

(defptype eql (value)
  `(eql ,value))

;; this type must be used to mark slots which might be h-unused (e.g. (or h-unused integer))
(defptype h-unused ()
  `(eql ,+h-unused-slot-marker+))

(defmapping h-unused :null
  'h-unused-reader
  'h-unused-writer)

(defmethod compute-type-tag ((type (eql 'h-unused)))
  3)

(eval-always
  (unless (member 'h-unused *mapped-type-precedence-list*)
    (setf *mapped-type-precedence-list*
          (cons (car *mapped-type-precedence-list*)
                (cons 'h-unused
                      (cdr *mapped-type-precedence-list*)))))
  (pushnew 'h-unused *canonical-types*))
