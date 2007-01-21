;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(defun oid-matcher-where-clause (object id-name)
  (make-instance 'sql-binary-operator
                 :name '=
                 :left (make-instance 'sql-identifier
                                      :name id-name)
                 :right (make-instance 'sql-literal
                                       :type +oid-id-sql-type+
                                       :value (id-of object))))

(defun oid-matcher-reader-where-clause (object)
  (oid-matcher-where-clause object +id-column-name+))

(defun oid-matcher-writer-where-clause (object value)
  (declare (ignore value))
  (oid-matcher-where-clause object +id-column-name+))

(defun value-matcher-writer-where-clause (object value)
  (declare (ignore object))
  (oid-matcher-where-clause value +id-column-name+))

(defun make-value-oid-matcher-writer-where-clause (id-name)
  (lambda (object value)
    (value-oid-matcher-writer-where-clause id-name object value)))

(defun value-oid-matcher-writer-where-clause (id-name object value)
  (if value
      (oid-matcher-where-clause value +id-column-name+)
      (oid-matcher-where-clause object id-name)))

(defun make-association-end-matcher-reader-where-clause (id-name)
  (lambda (object)
    (oid-matcher-where-clause object id-name)))

(defun make-association-end-matcher-writer-where-clause (id-name)
  (lambda (object value)
    (declare (ignore value))
    (oid-matcher-where-clause object id-name)))

(defun make-list-matcher-writer-where-clause (id-name)
  (lambda (object value)
    (list-matcher-writer-where-clause id-name object value)))

(defun list-matcher-writer-where-clause (id-name object values)
  (declare (ignore object))
  (make-instance 'sql-binary-operator
                 :name 'in
                 :left (make-instance 'sql-identifier :name id-name)
                 :right (mapcar (lambda (value)
                                  (make-instance 'sql-literal
                                                 :type +oid-id-sql-type+
                                                 :value (id-of value)))
                                values)))
