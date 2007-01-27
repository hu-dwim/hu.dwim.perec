;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(defcclass* exportable ()
  ((ensure-exported
    (compute-as (export-to-rdbms -self-) -self-)
    :reader ensure-exported
    :documentation "A persistent class, a persistent association and the related tables must be exported before use. This will automatically happen not later than making, reviving, querying or using by any means the first instance of it.")))

(defgeneric export-to-rdbms (object)
  (:documentation "Exports classes, associations, tables to the database, may create new tables or alter existing ones."))
