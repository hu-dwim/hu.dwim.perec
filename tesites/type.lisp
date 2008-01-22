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
  (h-unused-reader #L(error 'type-error :datum (first !1) :expected-type type))
  (h-unused-writer #L(error 'type-error :datum !1 :expected-type type)))
