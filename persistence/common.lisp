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

(defgeneric export-to-rdbms (object)
  (:documentation "Exports classes, associations, tables to the database, may create new tables or alter existing ones."))
