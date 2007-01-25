;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(defmacro defcclass* (name superclasses slots &rest options)
  `(defclass* ,name ,superclasses , slots
    ,@(append (unless (find :metaclass options :key 'first)
                '((:metaclass computed-class)))
              options)))

(defcfun (ensure-exported :computed-in compute-as) (object)
  "A persistent class, a persistent association and the related tables must be exported before use. This will automatically happen not later than making, reviving, querying or using by any means the first instance of it."
  (export-to-rdbms object)
  object)

(defgeneric export-to-rdbms (object)
  (:documentation "Exports classes, associations, tables to the database, may create new tables or alter existing ones."))
