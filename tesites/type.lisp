;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

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
  :null)

(eval-always
  (unless (member 'h-unused *mapped-type-precedence-list*)
    (setf *mapped-type-precedence-list*
          (cons (car *mapped-type-precedence-list*)
                (cons 'h-unused
                      (cdr *mapped-type-precedence-list*)))))
  (pushnew 'h-unused *canonical-types*))
